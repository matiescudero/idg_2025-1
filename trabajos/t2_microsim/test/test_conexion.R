# pruebas/test_conexion.R

# Paso 0: Instalar paquete dentro de entorno local
# install.packages("RPostgres")


library(RPostgres)
library(rakeR)
library(DBI)
library(ggplot2)

source("trabajos/t2_microsim/R/conexion_db.R")

con = conectar_db("censo_rm_2017")

# ---- Paso 2: Leer y ejecutar el query del CENSO ----
query_censo <- readLines("trabajos/t2_microsim/data/query_censo.sql") |> paste(collapse = "\n")
cons_censo_df <- dbGetQuery(con, query_censo)


# --- arreglar integer64 a integer ---
is64 <- sapply(cons_censo_df, function(x) inherits(x,"integer64"))
for(col in names(cons_censo_df)[is64]) {
  cons_censo_df[[col]] <- as.integer(cons_censo_df[[col]])
}


# Guardar el archivo
saveRDS(cons_censo_df, file = "trabajos/t2_microsim/data/cons_censo_df.rds")







# 1) Ordenar y extraer una sola vez los nombres de las columnas de constraints
col_cons   <- sort(setdiff(names(cons_censo_df), c("GEOCODIGO","COMUNA")))

# 2) De ahí generar dinámicamente los niveles que luego deben coincidir con los factor levels
age_levels  <- grep("^edad", col_cons, value = TRUE)    # p.ej. "edad_menor_30", "edad_30_40", …
esc_levels  <- grep("^esco", col_cons, value = TRUE)    # p.ej. "esco_0","esco_1_8",…
sexo_levels <- grep("^sexo_",col_cons, value = TRUE)    # p.ej. "sexo_f","sexo_m"






# crear la lista de constraints POR COMUNA
cons_censo_comunas <- split(cons_censo_df, cons_censo_df$COMUNA)

ruta_casen <- "data/casen_rm.rds"
casen_raw <- readRDS(ruta_casen)


# 2) Quedarnos sólo con las variables base
vars_base <- c("estrato",
               "esc",        # años de escolaridad
               "edad",       # Edad
               "educ",       # nivel de escolaridad
               "sexo",       # código de sexo
               "e6a",
               "ypc")        # predictor para imputar

casen <- casen_raw[ , vars_base, drop = FALSE]
rm(casen_raw)  # liberamos memoria



# 3) Extraer COMUNA y descartar 'estrato'
casen$Comuna  <- substr(as.character(casen$estrato), 1, 5)
casen$estrato <- NULL

# ————————————————————————
# 4) Quitar etiquetas haven_labelled y forzar atómicos
casen$esc  <- as.integer(unclass(casen$esc))
casen$edad  <- as.integer(unclass(casen$edad))
casen$educ <- as.integer(unclass(casen$educ))
casen$sexo <- as.integer(unclass(casen$sexo))
casen$e6a  <- as.numeric(unclass(casen$e6a))
casen$ypc <- as.numeric(unclass(casen$ypc))


# 5) Limpiar e imputar 'esc' (Años de escolaridad)
#    – Fuera de rango → NA
casen$esc[casen$esc < 0 | casen$esc > 29] <- NA

#    – Imputación lineal por e6a
imputar_escolaridad <- function(df) {
  idx_na <- which(is.na(df$esc))
  if (length(idx_na) == 0) return(df)
  fit   <- lm(esc ~ e6a, df[-idx_na, ], na.action = na.omit)
  pred  <- predict(fit, df[idx_na, , drop = FALSE])
  pred  <- pmax(0, pmin(29, pred))
  df$esc[idx_na] <- as.integer(round(pred))
  df
}
casen <- imputar_escolaridad(casen)

# 6) Añadir ID fijo
casen$ID <- as.character(seq_len(nrow(casen)))



# 7) Guardar snapshot base
casen_base <- casen[ , c("ID","Comuna","edad","esc","sexo","e6a","ypc")]
saveRDS(casen_base, "trabajos/t2_microsim/data/casen_base_preprocesado.rds")




# Después acá cargamos los RDS que guardamos, por mientras usamos los que están ya cargados.


casen_pob   <- casen_base

# 3) Recodificar para rakeR
casen_pob$edad_cat <- cut(
  casen_pob$edad,
  breaks = c(0,30,40,50,60,70,80,Inf),
  labels = age_levels,
  right = FALSE, include.lowest = TRUE
)

casen_pob$esc_cat <- factor(
  with(casen_pob,
       ifelse(esc == 0,           esc_levels[1],
              ifelse(esc <= 8,    esc_levels[2],
                     ifelse(esc <= 12, esc_levels[3],
                            esc_levels[4])))),
  levels = esc_levels
)

casen_pob$sexo_cat <- factor(
  ifelse(casen_pob$sexo == 2, sexo_levels[1],  # 2→"sexo_f"
         ifelse(casen_pob$sexo == 1, sexo_levels[2], NA)), # 1→"sexo_m"
  levels = sexo_levels
)

inds_list <- split(casen_pob, casen_pob$Comuna)

# 4) Preparar lista de inds por comuna


sim_list <- lapply(names(cons_censo_comunas), function(zona) {
  cons_i    <- cons_censo_comunas[[zona]]
  col_order <- sort(setdiff(names(cons_i), c("COMUNA","GEOCODIGO")))
  cons_i    <- cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  
  tmp    <- inds_list[[zona]]
  inds_i <- tmp[, c("ID","edad_cat","esc_cat","sexo_cat"), drop = FALSE]
  names(inds_i) <- c("ID","Edad","Escolaridad","Sexo")
  
  w_frac  <- weight(cons = cons_i, inds = inds_i,
                    vars = c("Edad","Escolaridad","Sexo"))
  sim_i   <- integerise(weights = w_frac, inds = inds_i, seed = 123)
  merge(sim_i,
        tmp[, c("ID","ypc")],
        by = "ID", all.x = TRUE)
})

sim_df <- data.table::rbindlist(sim_list, idcol = "COMUNA")


# Asumo que en sim_df la columna "zone" es tu GEOCODIGO
agg_income <- sim_df %>%
  group_by(geocodigo = as.numeric(zone)) %>%
  summarise(
    ingreso_promedio = median(ypc, na.rm = TRUE),
    poblacion_sim = n()
  ) %>%
  ungroup()

dbWriteTable(
  conn      = con,
  name      = Id(schema = "dpa", table = "tmp_ingreso_rm"),
  value     = agg_income,
  overwrite = TRUE,
  row.names = FALSE
)

dbExecute(con, "CREATE INDEX ON dpa.tmp_ingreso_rm(geocodigo)")
dbExecute(con, "ANALYZE dpa.tmp_ingreso_rm")

# 1) Crea la nueva capa directamente con un SELECT … LEFT JOIN
dbExecute(con, "
  CREATE TABLE dpa.zonas_censales_rm_income AS
  SELECT
    z.*,
    t.ingreso_promedio,
    t.poblacion_sim
  FROM dpa.zonas_censales_rm AS z
  LEFT JOIN dpa.tmp_ingreso_rm AS t
    ON z.geocodigo = t.geocodigo
")

# 2) Indexa para acelerar lecturas y renderizado en QGIS
dbExecute(con, "CREATE INDEX ON dpa.zonas_censales_rm_income(geocodigo)")
dbExecute(con, "CREATE INDEX ON dpa.zonas_censales_rm_income USING GIST(geom)")
dbExecute(con, "ANALYZE dpa.zonas_censales_rm_income")





# 1) Promedio “real” de Casen por comuna
casen_income_comuna <- casen_pob %>%
  group_by(Comuna) %>%
  summarise(
    ingreso_casen = mean(ypc, na.rm = TRUE),
    n_casen       = n()
  )

# 1) Extrae el código de comuna de 5 dígitos
sim_df2 <- sim_df %>%
  mutate(
    Comuna5 = substr(as.character(zone), 1, 5)
  )

# 2) Agrupa por esa comuna y calcula los promedios
sim_income_comuna2 <- sim_df2 %>%
  group_by(Comuna = Comuna5) %>%
  summarise(
    ingreso_sim   = mean(ypc, na.rm = TRUE),
    poblacion_sim = n(),
    .groups = "drop"
  )

# 3) Asegúrate de que el tipo coincide con tu casen_income_comuna
sim_income_comuna2 <- sim_income_comuna2 %>%
  mutate(Comuna = as.character(Comuna))

# 4) Ahora el join cabalga
cmp2 <- casen_income_comuna %>%
  right_join(sim_income_comuna2, by = "Comuna")

head(cmp2)

ggplot(cmp2, aes(x = ingreso_casen, y = ingreso_sim)) +
  geom_abline(lty = 2, colour = "grey50") +
  geom_point(aes(size = n_casen), alpha = .7) +
  geom_text(aes(label = Comuna), check_overlap = TRUE, size = 3) +
  scale_size_area() +
  labs(
    x = "Ingreso promedio Casen (real)",
    y = "Ingreso promedio microsimulado",
    size = "N observados\n(Casen)"
  ) +
  theme_minimal()


real_q <- quantile(casen_pob$ypc, probs = seq(0,1,0.1), na.rm = TRUE)
sim_q  <- quantile(sim_df$ypc,   probs = seq(0,1,0.1), na.rm = TRUE)
data.frame(real_q, sim_q)


cmp2 <- cmp2 %>% 
  mutate(
    error = ingreso_sim - ingreso_casen,
    pct_error = error / ingreso_casen * 100
  )
summary(cmp2$error)
summary(cmp2$pct_error)

ggplot(cmp2, aes(x = (ingreso_casen + ingreso_sim)/2, y = ingreso_sim - ingreso_casen)) +
  geom_point() +
  geom_hline(yintercept = mean(cmp2$error), lty = 2) +
  geom_hline(yintercept = mean(cmp2$error) + 1.96*sd(cmp2$error), col = "red") +
  geom_hline(yintercept = mean(cmp2$error) - 1.96*sd(cmp2$error), col = "red") +
  labs(x = "Media real/simulada", y = "Diferencia simulada – real") +
  theme_minimal()
