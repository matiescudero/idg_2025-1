# Paso 0: Cargar librerías
library(sf)
library(ggplot2)
library(viridis)
library(dplyr)
library(DBI)
library(RPostgres)
library(ggspatial)
library(raster)
library(ggmap)

# Paso 1: Conectarse a la base de datos
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "censo_v_2017",
  host = "localhost",
  port = 5434,
  user = "postgres",
  password = "postgres"
)

# Paso 2: Cargar zonas censales y comunas desde PostgreSQL
zonas_prof <- st_read(con, query = "SELECT * FROM output.tot_prof_geom")
comunas <- st_read(con, query = "SELECT nom_comuna, geom FROM dpa.comunas_v")

# Paso 3: Cerrar conexión
dbDisconnect(con)

# Paso 4: Validar geometrías
zonas_prof <- st_make_valid(zonas_prof)
comunas <- st_make_valid(comunas)

# Paso 5: Definir el polígono de extensión (WKT) (https://arthur-e.github.io/Wicket/sandbox-gmaps3.html)
wkt_extent <- "POLYGON((
  -71.73302689517163 -32.90522006089673,
  -71.44600907290601 -32.90522006089673,
  -71.44600907290601 -33.12171365341705,
  -71.73302689517163 -33.12171365341705,
  -71.73302689517163 -32.90522006089673
))"


extent_geom <- st_as_sfc(wkt_extent, crs = 4326)

# Paso 6: Recortar zonas censales
zonas_continental <- zonas_prof %>%
  st_filter(extent_geom)

# Paso 7: Recortar comunas basándonos en centroides
centroides_comunas <- st_centroid(comunas)

comunas_continental <- comunas[st_within(centroides_comunas, extent_geom, sparse = FALSE), ]

# Paso 8: Mapa actualizado
ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 12) +  # Zoom más grande porque es área pequeña
  geom_sf(data = zonas_continental, aes(fill = total_profesionales), color = NA, alpha = 0.8) +
  geom_sf(data = comunas_continental, fill = NA, color = "black", size = 0.5) +  # Bordes comunales
  geom_sf_text(data = comunas_continental, aes(label = nom_comuna), size = 3, color = "black", fontface = "bold") +  # Nombres
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(
    title = "Porcentaje de Profesionales - Valparaíso y Viña del Mar",
    subtitle = "Fuente: Censo de Población y Vivienda, mapa base CartoLight",
    fill = "% Profesionales"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )


# Definir el zoom exacto
bbox <- c(left = -71.733, bottom = -33.1217, right = -71.446, top = -32.9052)

ggplot() +
  geom_sf(data = zonas_prof, aes(fill = total_profesionales), color = "grey80", size = 0.2, alpha = 0.9) +
  geom_sf(data = comunas, fill = NA, color = "black", size = 0.8) +
  geom_sf_text(data = comunas, aes(label = nom_comuna), size = 3, color = "black", fontface = "bold") +
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "white") +
  coord_sf(
    xlim = c(-71.733, -71.446),
    ylim = c(-33.1217, -32.9052),
    expand = FALSE
  ) +
  labs(
    title = "Porcentaje de Profesionales - Zoom en Valparaíso y Viña del Mar",
    subtitle = "Fuente: Censo de Población y Vivienda",
    fill = "% Profesionales"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )