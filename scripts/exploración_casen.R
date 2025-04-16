# exploracion_casen.R
# ===============================================
# Script para explorar la base de datos CASEN filtrada para la Región Metropolitana (RM)
# Este script realiza:
#   1. Lectura y filtrado de la base de datos
#   2. Exploración de las variables: nombres, estructura, resumen y correlaciones
#   3. Visualizaciones básicas (histogramas y boxplots)
# ===============================================

# 1. Lectura del archivo RDS
ruta_rds = "data/casen_rm.rds"
casen_rm = readRDS(ruta_rds)

# 2. Visualización de variables clave

## 2.1 Histograma de Ingreso Total Corregido


hist(casen_rm$ytotcor,
     main = "Histograma de Ingreso Total Corregido",
     xlab = "Ingreso Total Corregido",
     col = "lightblue", border = "white")


## 2.2. Histograma de Edad
hist(casen_rm$edad,
     main = "Histograma de Edad",
     xlab = "Edad",
     col = "lightgreen", border = "white")


# 3. Análisis Exploratorio Ingreso Corregido

## Analizar Boxplot a ver si dice algo
boxplot(casen_rm$ytotcor,
        main = "Boxplot de ytotcor (antes de limpiar)",
        ylab = "ytotcor",
        col = "lightblue")


# Vemos el percentil para filtrar en base a eso
umbral = quantile(casen_rm$ytotcor, 0.9, na.rm = TRUE)


# Filtrar los registros que están por debajo o igual al umbral (eliminar outliers)
casen_rm_clean = casen_rm[casen_rm$ytotcor <= umbral, ]


boxplot(casen_rm_clean$ytotcor,
        main = "Boxplot de ytotcor (después de limpiar)",
        ylab = "ytotcor",
        col = "lightblue")

hist(casen_rm_clean$ytotcor,
     main = "Histograma de Ingreso Total Corregido",
     xlab = "Ingreso Total Corregido",
     col = "lightblue", border = "white")


# Se filtra para  quedarse solo con registros donde ytotcor es mayor a 0
casen_rm_no0 = casen_rm_clean[casen_rm_clean$ytotcor > 0,]

# Visualizar la distribución sin 0's
hist(casen_rm_no0$ytotcor, 
     main = "Histograma de ytotcor sin ceros", 
     xlab = "ytotcor", col = "lightblue", border = "white")


# Boxplot sin los 0's
boxplot(casen_rm_no0$ytotcor,
        main = "Boxplot de ytotcor sin ceros",
        ylab = "ytotcor", col = "lightgreen")


# Visualizar la distribución sin 0's y con logaritmo
hist(log(casen_rm_no0$ytotcor), 
     main = "Histograma de ytotcor sin ceros", 
     xlab = "ytotcor", col = "lightblue", border = "white")



#### NUEVO DF ####

# Definir las variables de interés
vars_interes <- c("yautcorh", "ytotcorh", "ypc", "ypchtrabcor",
                  "esc", "nse", "edad", "sexo")

# Crear el nuevo data frame con solo esas columnas
df_interes <- casen_rm_no0[, vars_interes]

write.csv(df_interes, file = "data/df_interes.csv", row.names = FALSE)


