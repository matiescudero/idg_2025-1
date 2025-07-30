library(haven)
library(dplyr)
library(ggplot2)
library(MetBrewer)

# Leer archivos Stata
personas <- read_dta("data/datos_epf/base-personas-ix-epf-stata.dta")
gastos   <- read_dta("data/datos_epf/base-gastos-ix-epf-stata.dta")
cantidades <- read_dta("data/datos_epf/base-cantidades-ix-epf-stata.dta")
ccif     <- read_dta("data/datos_epf/ccif-ix-epf-stata.dta")


# Asegurarse de tener un df con cse por folio
personas_cse <- personas %>%
  select(folio, cse) %>%
  distinct()

# Unir cse a gastos
gastos_cse <- gastos %>%
  left_join(personas_cse, by = "folio")

# Unir glosa de división desde ccif (como hiciste antes)
ccif_div <- ccif %>%
  group_by(d) %>%
  slice(1) %>%
  select(d, glosa_ccif) %>%
  rename(glosa_division = glosa_ccif)

# Unir glosa de división
gastos_cse <- gastos_cse %>%
  left_join(ccif_div, by = "d")

# Calcular gasto promedio por cse y división
gasto_por_cse <- gastos_cse %>%
  group_by(cse, glosa_division) %>%
  summarise(gasto_promedio = mean(gasto, na.rm = TRUE), .groups = "drop")

ggplot(gasto_por_cse, aes(x = factor(cse), y = gasto_promedio, fill = glosa_division)) +
  geom_col(position = "dodge") +
  labs(title = "Gasto promedio por división de consumo y estrato socioeconómico (CSE)",
       x = "Estrato socioeconómico (CSE)", y = "Gasto promedio (CLP)", fill = "División CCIF") +
  theme_minimal()

gasto_por_cse_prop <- gastos_cse %>%
  group_by(cse, glosa_division) %>%
  summarise(gasto = sum(gasto, na.rm = TRUE), .groups = "drop") %>%
  group_by(cse) %>%
  mutate(prop = gasto / sum(gasto, na.rm = TRUE))

# Crear colores planos para cada categoría (13 o los que tengas)
colores_discretos <- c(
  "#E63946", "#F1A208", "#2A9D8F", "#264653", "#8E44AD",
  "#F4A261", "#A8DADC", "#1D3557", "#FF6B6B", "#6D597A",
  "#B56576", "#355070", "#FFB703"
)

# Plot apilado proporcional
ggplot(gasto_por_cse_prop, aes(x = factor(cse), y = prop, fill = glosa_division)) +
  geom_col(position = "fill", color = "white") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = colores_discretos) +
  labs(title = "Proporción del gasto mensual por división y estrato socioeconómico (CSE)",
       x = "Estrato socioeconómico (CSE)", y = "Proporción del gasto", fill = "División CCIF") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        panel.grid.major.y = element_line(color = "gray90"))
