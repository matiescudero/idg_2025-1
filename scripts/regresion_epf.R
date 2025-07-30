library(haven)
library(dplyr)
library(ggplot2)
library(MetBrewer)

# Leer archivos Stata
personas <- read_dta("data/datos_epf/base-personas-ix-epf-stata.dta")
gastos   <- read_dta("data/datos_epf/base-gastos-ix-epf-stata.dta")
cantidades <- read_dta("data/datos_epf/base-cantidades-ix-epf-stata.dta")
ccif     <- read_dta("data/datos_epf/ccif-ix-epf-stata.dta")

# Se fitran registros de personas para el Gran Santiago
personas_gs = personas[personas$macrozona == 2, ]



# Filtro para valores inválidos.
valores_invalidos <- c(-99, -88, -77)


# Edad y escolaridad
personas_gs = personas_gs[!(personas_gs$edad %in% valores_invalidos), ]
personas_gs = personas_gs[!(personas_gs$edue %in% valores_invalidos), ]
personas_gs = personas_gs[!(personas_gs$ing_disp_hog_hd_ai < 0), ]

# Se calcula el ingreso per cápita
personas_gs$ing_pc = personas_gs$ing_disp_hog_hd_ai / personas_gs$npersonas

# Filtrar base cantidades: gastos en productos tipo "pan"
cantidades_pan <- cantidades[
  cantidades$g == "1" &
    cantidades$c == "1" &
    cantidades$sc == "03" &
    (cantidades$p == "01" | cantidades$p == "05" | cantidades$p == "06") &
    !is.na(cantidades$gasto),
]


# Sumar gasto total en pan por hogar
gasto_hogar <- aggregate(cantidades_pan$gasto,
                         by = list(folio = cantidades_pan$folio),
                         FUN = sum)
colnames(gasto_hogar)[2] <- "gasto_pan"

# Filtrar solo a jefe/a de hogar
personas_jefe <- personas_gs[personas_gs$sprincipal == 1, ]

# 7. Unir gasto con datos sociodemográficos
datos_modelo <- merge(gasto_hogar, personas_jefe, by = "folio")
datos_modelo <- datos_modelo[datos_modelo$ing_pc >= 0, ]



hist(datos_modelo$gasto_pan, breaks = 50)
boxplot(datos_modelo$gasto_pan, horizontal = TRUE)

plot(datos_modelo$edad, datos_modelo$gasto_pan)
plot(datos_modelo$edue, datos_modelo$gasto_pan)
plot(datos_modelo$ing_pc, datos_modelo$gasto_pan)



q95_pan = quantile(datos_modelo$gasto_pan, 0.95)
q95_ingreso = quantile(datos_modelo$ing_pc, 0.95)

datos_filtrados <- datos_modelo[datos_modelo$gasto_pan <= q95_pan, ]
datos_filtrados <- datos_modelo[datos_modelo$ing_pc <= q95_ingreso, ]

hist(datos_filtrados$ing_pc, breaks = 50)
hist(datos_filtrados$gasto_pan, breaks = 50)

plot(datos_filtrados$edad, datos_filtrados$gasto_pan,
     main = "Edad vs Gasto en Pan", xlab = "Edad", ylab = "Gasto en Pan", pch = 20)
lines(lowess(datos_filtrados$edad, datos_filtrados$gasto_pan), col = "red", lwd = 2)

boxplot(gasto_pan ~ edue, data = datos_filtrados,
        main = "Gasto en Pan según Escolaridad", xlab = "Años de Escolaridad", ylab = "Gasto en Pan")

plot(log1p(datos_filtrados$ing_pc), datos_filtrados$gasto_pan,
     main = "Log(ing_pc + 1) vs Gasto en Pan", xlab = "Log(Ingreso per cápita + 1)", ylab = "Gasto en Pan", pch = 20)
lines(lowess(log1p(datos_filtrados$ing_pc), datos_filtrados$gasto_pan), col = "blue", lwd = 2)


# 8. Ajustar regresión
modelo_pan <- lm(gasto_pan ~ edad + edue + ing_pc, data = datos_filtrados)
summary(modelo_pan)


# Transformación logarítmica del ingreso
datos_filtrados$log_ing_pc <- log1p(datos_filtrados$ing_pc)
datos_filtrados$log_gasto_pan <- log1p(datos_filtrados$gasto_pan)

# Variable cuadrática
datos_filtrados$edad2 <- datos_filtrados$edad^2

# Agrupar escolaridad (opcional)
datos_filtrados$grupo_escolaridad <- cut(
  datos_filtrados$edue,
  breaks = c(-1, 8, 12, 16, 24),
  labels = c("Baja", "Media-baja", "Media-alta", "Alta")
)

# Modelo propuesto
modelo <- lm(
  log_gasto_pan ~ edad + log_ing_pc + grupo_escolaridad,
  data = datos_filtrados
)

summary(modelo)
