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
               "pobreza_multi_5d")        # predictor para imputar

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
casen$pobreza_multi_5d <- as.numeric(unclass(casen$pobreza_multi_5d))


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
casen_base <- casen[ , c("ID","Comuna","edad","esc","sexo","e6a","pobreza_multi_5d")]
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

casen_pob <- casen_pob %>%
  mutate(
    pobreza_cat = factor(
      ifelse(pobreza_multi_5d == 1, "Pobre", "NoPobre"),
      levels = c("NoPobre","Pobre")
    )
  )


inds_list <- split(casen_pob, casen_pob$Comuna)

# 4) Preparar lista de inds por comuna


sim_list <- lapply(names(cons_censo_comunas), function(zona) {
  # 1.a) marginals censales
  cons_i    <- cons_censo_comunas[[zona]]
  col_order <- setdiff(names(cons_i), c("COMUNA","GEOCODIGO"))
  cons_i    <- cons_i[, c("GEOCODIGO", sort(col_order)), drop = FALSE]
  
  # 1.b) individuos Casen para esa comuna
  tmp   <- inds_list[[zona]]
  inds_i <- tmp[, c("ID","edad_cat","esc_cat","sexo_cat"), drop = FALSE]
  names(inds_i) <- c("ID","Edad","Escolaridad","Sexo")
  
  # 1.c) IPF + integerise sobre Edad×Escolaridad×Sexo
  w_frac <- weight(cons = cons_i, inds = inds_i,
                   vars = c("Edad","Escolaridad","Sexo"))
  sim_i  <- integerise(weights = w_frac, inds = inds_i, seed = 123)
  
  # 1.d) Ahora une la variable de pobreza de Casen
  merge(sim_i,
        tmp[, c("ID","pobreza_multi_5d")],
        by = "ID",
        all.x = TRUE)
})

sim_df <- data.table::rbindlist(sim_list, idcol = "COMUNA")


# 2) Agregas porcentaje de pobres por zona censal (GEOCODIGO)
poverty_zonal <- sim_df %>%
  group_by(geocodigo = as.numeric(zone)) %>%
  summarise(
    total_sim  = n(),
    n_pobres   = sum(pobreza_multi_5d == 1, na.rm = TRUE),
    pct_pobres = n_pobres / total_sim * 100,
    .groups    = "drop"
  )

# 3) Subes la tabla temporal a la BD
dbWriteTable(
  con,
  name      = DBI::Id(schema = "dpa", table = "tmp_pobreza_zonal"),
  value     = poverty_zonal,
  overwrite = TRUE,
  row.names = FALSE
)
dbExecute(con, "CREATE INDEX ON dpa.tmp_pobreza_zonal(geocodigo)")
dbExecute(con, "ANALYZE dpa.tmp_pobreza_zonal")

# 4) Creas la capa nueva con el porcentaje de pobreza
dbExecute(con, "
  CREATE TABLE dpa.zonas_censales_rm_pobreza AS
  SELECT
    z.*,
    t.pct_pobres
  FROM dpa.zonas_censales_rm AS z
  LEFT JOIN dpa.tmp_pobreza_zonal AS t
    ON z.geocodigo = t.geocodigo
")
dbExecute(con, "CREATE INDEX ON dpa.zonas_censales_rm_pobreza(geocodigo)")
dbExecute(con, "CREATE INDEX ON dpa.zonas_censales_rm_pobreza USING GIST(geom)")
dbExecute(con, "ANALYZE dpa.zonas_censales_rm_pobreza")
