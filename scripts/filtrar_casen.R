# scripts/filtrar_casen.R
# ============================================
# Script para leer, filtrar y exportar la encuesta CASEN 2022
# Filtra la encuesta para la Región Metropolitana (RM) y guarda el resultado en formato RDS.
# ============================================

# Se cargan librerías necesarias
library(haven)


# 1. Configuración
ruta_archivo = "encuestas/CASEN/Base de datos Casen 2022 SPSS_18 marzo 2024.sav"
ruta_salida = "data/casen_rm.rds"

# 2. Procesamiento
# Lectura del dataset
casen_completa = read_sav(ruta_archivo)

# Se filtra la encuesta únicamente para la RM
casen_rm = casen_completa[casen_completa$region == 13, ]

# Se elimina el objeto de casen_completa para liberar espacio
rm(casen_completa)
gc()

# Exportar como archivo .rds
saveRDS(casen_rm, file = ruta_salida)
