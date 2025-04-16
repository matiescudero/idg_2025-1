# 1. Entradas

ruta_rds = "data/casen_rm.rds"
casen_rm = readRDS(ruta_rds)

# 2. Visualización Variables

## Ingreso per cápita

hist(casen_rm$ypc,
     xlab = "Ingreso Per Cápita",
     col = "lightblue")

boxplot(casen_rm$ypc)


umbral = quantile(casen_rm$ypc, 0.9, na.rm = TRUE)

casen_clean = casen_rm[(casen_rm$ypc <= umbral) & (casen_rm$edad >= 15), ]

boxplot(casen_clean$ypc)
hist(casen_clean$ypc)
