library(haven)

# Leer archivos Stata
personas <- read_dta("data/datos_epf/base-personas-ix-epf-stata.dta")
gastos   <- read_dta("data/datos_epf/base-gastos-ix-epf-stata.dta")
cantidades <- read_dta("data/datos_epf/base-cantidades-ix-epf-stata.dta")
ccif     <- read_dta("data/datos_epf/ccif-ix-epf-stata.dta")

# Se filtran registros de personas para el Gran Santiago
personas_gs = personas[personas$macrozona == 2 &
                         personas$sprincipal == 1, ]

# Filtro para valores inválidos.
valores_invalidos <- c(-99, -88, -77)

# Edad y escolaridad
personas_gs = personas_gs[!(personas_gs$edad %in% valores_invalidos), ]
personas_gs = personas_gs[!(personas_gs$edue %in% valores_invalidos), ]
personas_gs = personas_gs[!(personas_gs$ing_disp_hog_hd_ai < 0), ]

# Se calcula el ingreso per cápita
personas_gs$ing_pc = personas_gs$ing_disp_hog_hd_ai / personas_gs$npersonas

gastos_servicio = subset(gastos, ccif == "11.1.1.01.01" & macrozona == 2)
gasto_hogar_servicio = merge(gastos_servicio, personas_gs, by = "folio")
tabla_gastos = gasto_hogar_servicio[, c("sexo", "edad", "edue", "fe.x", "cse", "ing_pc", "gasto")]

# --- GRAFICOS EXPLORATORIOS ---
hist(tabla_gastos$ing_pc, breaks = 30, col = "lightblue", main = "Distribución del Ingreso", xlab = "Ingreso per cápita")
hist(tabla_gastos$gasto, breaks = 30, col = "lightblue", main = "Distribución del Gasto en Restorán", xlab = "Gasto en gimnasio")

boxplot(gasto ~ factor(sexo), data = tabla_gastos, main = "Gasto en Restoran según Sexo", xlab = "Sexo", col = c("tomato", "lightgreen"))

plot(tabla_gastos$edad, tabla_gastos$gasto, main = "Edad vs Gasto", xlab = "Edad", ylab = "Gasto", pch = 20, col = rgb(0,0,0,0.3))
lines(lowess(tabla_gastos$edad, tabla_gastos$gasto), col = "red", lwd = 2)

plot(tabla_gastos$ing_pc, tabla_gastos$gasto, main = "Ingreso vs Gasto", xlab = "Ingreso per cápita", ylab = "Gasto", pch = 20, col = rgb(0,0,0,0.3))
lines(lowess(tabla_gastos$ing_pc, tabla_gastos$gasto), col = "blue", lwd = 2)

# Escolaridad agrupada
tabla_gastos$grupo_escolaridad <- cut(tabla_gastos$edue, breaks = c(-Inf, 8, 12, 16, Inf), labels = c("Básica o menos", "Media-baja", "Media-alta", "Alta"), right = TRUE)
boxplot(gasto ~ grupo_escolaridad, data = tabla_gastos, main = "Gasto según Escolaridad", xlab = "Escolaridad", col = "skyblue")

# Agrupación edad
tabla_gastos$grupo_edad <- cut(tabla_gastos$edad, breaks = c(0, 29, 39, 49, 59, 69, 120), labels = c("0–29", "30–39", "40–49", "50–59", "60–69", "70+"), right = TRUE, include.lowest = TRUE)

# Logs para regresión
tabla_gastos$log_gasto  <- log1p(tabla_gastos$gasto)
tabla_gastos$log_ing_pc <- log1p(tabla_gastos$ing_pc)

# --- REGRESIÓN LINEAL ---
modelo_restoran <- lm(log_gasto ~ sexo + grupo_edad + log_ing_pc + grupo_escolaridad, data = tabla_gastos)
summary(modelo_restoran)

