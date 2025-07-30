# scripts/test_procesar_casen_estatico.R

library(dplyr)

# 1. Leer RDS
casen_raw <- readRDS("data/casen_rm.rds")

# 2. Echar un vistazo
glimpse(casen_raw)
summary(casen_raw$escolaridad)
table(casen_raw$p08)        # Sexo
table(casen_raw$comuna)     # Comuna
table(casen_raw$e6a)        # Años de educación
table(casen_raw$s28)        # Enfermedad
