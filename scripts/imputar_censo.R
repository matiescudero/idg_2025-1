library(RPostgres)
library(dplyr)

# =============================================================================
# 3) CONFIGURAR CONEXIÓN A BASE DE DATOS
# =============================================================================
# Definir parámetros de conexión
db_host     = "localhost"       # servidor de BD
db_port     = 5434                # puerto de escucha
db_name     = "censo_rm_2017"   # nombre de la base
db_user     = "postgres"        # usuario de conexión
db_password = "postgres"        # clave de usuario

# Establecer conexión usando RPostgres
con = dbConnect(
  Postgres(),
  dbname   = db_name,
  host     = db_host,
  port     = db_port,
  user     = db_user,
  password = db_password
)

df <- dbGetQuery(con, "
  SELECT 
    persona_ref_id,
    escolaridad,
    p09 AS edad,
    p08 AS sexo,
    p15 AS nivel_historico,
    p13 AS en_educacion
  FROM personas
")

# Marca NA sólo en escolaridad y nivel_historico
df <- df %>%
  mutate(across(
    c(escolaridad, nivel_historico),
    ~ replace(.x, .x %in% c(98,99), NA_integer_)
  ))


library(naniar)
gg_miss_upset(df)    # visualiza patrones de faltantes por variable

library(mice)

# 1) Extrae primero el ID
df_id <- df %>% select(persona_ref_id)

# 2) Prepara solo las variables para imputar
datos_mice <- df %>% 
  select(escolaridad, nivel_historico, edad, sexo, en_educacion)

# 3) Imputa con mice como hiciste
imp <- mice(datos_mice, m = 1, method = "pmm", seed = 42)
df_imp_sin_id <- complete(imp)

# 4) Vuélvele a pegar el ID por posición
df_complete <- bind_cols(df_id, df_imp_sin_id)

# Comprueba
stopifnot(nrow(df_complete) == nrow(df))
stopifnot(sum(is.na(df_complete$escolaridad)) == 0)

# 3.1) Escribe tabla temporal
dbWriteTable(
  con, 
  "personas_imp_tmp", 
  df_complete %>% select(persona_ref_id, escolaridad_imp = escolaridad, nivel_historico_imp = nivel_historico),
  overwrite = TRUE
)


# 3.2) Actualiza la tabla original
dbExecute(con, "
  UPDATE personas p
  SET 
    escolaridad     = t.escolaridad_imp
  FROM personas_imp_tmp t
  WHERE p.persona_ref_id = t.persona_ref_id
")
