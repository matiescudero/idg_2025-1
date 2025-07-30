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


# Filtrar la base de cantidades en función de mi servicio
cantidades_servicio = cantidades[
  (cantidades$g == "4" &
    cantidades$c == "6" &
    cantidades$sc == "02" &
    cantidades$p == "04"),
  ]

gastos_servicio = 
  gastos[
    gastos$ccif == "09.4.6.02.04" &
      gastos$macrozona == 2,
  ]

# Sumar gasto total en el servicio por hogar
gasto_hogar_servicio = merge(gastos_servicio, personas_gs, by = "folio")

tabla_gastos = gasto_hogar_servicio[, c("sexo", "edad", "edue", "fe.x", 
                                        "cse", "ing_pc", "gasto")]

hist(tabla_gastos$ing_pc, breaks = 30, col = "lightblue",
     main = "Distribución del Ingreso",
     xlab = "Ingreso per cápita", ylab = "Frecuencia")

hist(tabla_gastos$gasto, breaks = 30, col = "lightblue",
     main = "Distribución del Gasto en Gimnasio",
     xlab = "Gasto en gimnasio", ylab = "Frecuencia")

boxplot(gasto ~ factor(sexo), data = tabla_gastos,
        main = "Gasto en Gimnasio según Sexo",
        xlab = "Sexo (1 = Hombre, 2 = Mujer)", ylab = "Gasto en gimnasio",
        col = c("tomato", "lightgreen"))

plot(tabla_gastos$edad, tabla_gastos$gasto,
     main = "Edad vs Gasto en Gimnasio",
     xlab = "Edad", ylab = "Gasto en gimnasio", pch = 20, col = rgb(0,0,0,0.3))
lines(lowess(tabla_gastos$edad, tabla_gastos$gasto), col = "red", lwd = 2)

plot(tabla_gastos$ing_pc, tabla_gastos$gasto,
     main = "Ingreso per cápita vs Gasto en Gimnasio",
     xlab = "Ingreso per cápita", ylab = "Gasto en gimnasio", pch = 20, col = rgb(0,0,0,0.3))
lines(lowess(tabla_gastos$ing_pc, tabla_gastos$gasto), col = "blue", lwd = 2)

boxplot(gasto ~ cut(edue, breaks = c(0, 8, 12, 16, 25),
                    labels = c("Básica o menos", "Media", "Técnica", "Universitaria")),
        data = tabla_gastos,
        main = "Gasto en Gimnasio según Escolaridad",
        xlab = "Grupos de escolaridad", ylab = "Gasto en gimnasio",
        col = "skyblue")



# Transformar variables
tabla_gastos$log_gasto  <- log1p(tabla_gastos$gasto)
tabla_gastos$log_ing_pc <- log1p(tabla_gastos$ing_pc)
tabla_gastos$edad2      <- tabla_gastos$edad^2


tabla_gastos$grupo_escolaridad <- cut(
  tabla_gastos$edue,
  breaks = c(-Inf, 8, 12, 16, Inf),
  labels = c("Básica o menos", "Media-baja", "Media-alta", "Alta"),
  right = TRUE
)
tabla_gastos$grupo_escolaridad <- factor(tabla_gastos$grupo_escolaridad)


tabla_gastos$grupo_edad <- cut(
  tabla_gastos$edad,
  breaks = c(0, 29, 39, 49, 59, 69, 120),
  labels = c("0–29", "30–39", "40–49", "50–59", "60–69", "70+"),
  right = TRUE, include.lowest = TRUE
)



# Modelo propuesto
modelo_gym <- lm(log_gasto ~ sexo + grupo_edad + log_ing_pc + grupo_escolaridad, data = tabla_gastos)
summary(modelo_gym)





modelo_int1 <- lm(log_gasto ~ sexo + grupo_edad + log_ing_pc * grupo_escolaridad, data = tabla_gastos)
summary(modelo_int1)

modelo_int2 <- lm(log_gasto ~ sexo + grupo_edad * grupo_escolaridad + log_ing_pc, data = tabla_gastos)

modelo_int3 <- lm(log_gasto ~ sexo + grupo_edad * log_ing_pc + grupo_escolaridad, data = tabla_gastos)

summary(modelo_int3)



AIC(modelo_gym, modelo_int1, modelo_int2, modelo_int3)
anova(modelo_gym, modelo_int1)  # Si modelo_int1 anida al original


anova(modelo_gym, modelo_int3)




library(ggplot2)

ggplot(tabla_gastos, aes(x = log_ing_pc, y = log_gasto, color = grupo_edad)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Interacción entre ingreso y edad en el gasto en gimnasio",
       x = "log(Ingreso per cápita)", y = "log(Gasto en gimnasio)") +
  theme_minimal()

library(mgcv)
modelo_gam <- gam(log_gasto ~ sexo + grupo_escolaridad + grupo_edad + s(log_ing_pc, by = grupo_edad), data = tabla_gastos)

summary(modelo_gam)




# 1. Total de gasto en gimnasio por hogar
gasto_gym_hogar <- aggregate(gasto ~ folio, data = gastos_servicio, FUN = sum)
gasto_gym_hogar$gasta_gym <- 1

# 2. Merge con personas
personas_gs <- merge(personas_gs, gasto_gym_hogar[, c("folio", "gasto", "gasta_gym")], 
                     by = "folio", all.x = TRUE)

# 3. Los que no tienen gasto aparecen como NA -> reemplazamos por 0
personas_gs$gasto[is.na(personas_gs$gasto)] <- 0
personas_gs$gasta_gym[is.na(personas_gs$gasta_gym)] <- 0


modelo_etapa1 <- glm(gasta_gym ~ sexo + edad + edue + log1p(ing_pc), 
                     data = personas_gs, family = binomial)
summary(modelo_etapa1)


# Probabilidad estimada de gastar en gimnasio
personas_gs$prob_predicha <- predict(modelo_etapa1, type = "response")

# Clasificación binaria con umbral 0.5
personas_gs$pred_clase <- ifelse(personas_gs$prob_predicha >= 0.5, 1, 0)

# Matriz de confusión
table(Real = personas_gs$gasta_gym, Predicha = personas_gs$pred_clase)

# Cálculo de accuracy
mean(personas_gs$gasta_gym == personas_gs$pred_clase)

library(pROC)

roc_obj <- roc(personas_gs$gasta_gym, personas_gs$prob_predicha)

# Plot ROC
plot(roc_obj, main = "Curva ROC - Modelo Logístico (Gasto en Gimnasio)", col = "darkblue")
auc(roc_obj)



# Evaluar a partir de un umbral más bajo, como 0.2
personas_gs$pred_clase_02 <- ifelse(personas_gs$prob_predicha >= 0.2, 1, 0)
table(Real = personas_gs$gasta_gym, Predicha = personas_gs$pred_clase_02)
mean(personas_gs$gasta_gym == personas_gs$pred_clase_02)

