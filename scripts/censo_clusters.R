# =============================================================================
# 2) CARGAR LIBRERÍAS NECESARIAS
# =============================================================================
library(factoextra)
library(ggfortify)
library(plotly)
library(DBI)
library(RPostgres)
library(sf)
library(ggplot2)
library(cowplot)
library(GGally)

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

# =============================================================================
# 4) EXTRAER INDICADORES DESDE CENSO
# =============================================================================
# SQL para calcular:
# - % de personas con nivel educativo profesional (p15 entre 12 y 14)
# - % de viviendas con indicadores de hacinamiento (v.ind_hacin_rec en {2,4})

sql_indicadores = "
SELECT
  z.geocodigo::double precision AS geocodigo,
  c.nom_comuna,

  -- Porcentaje de migrantes
  ROUND(
    COUNT(*) FILTER (WHERE p.p12 NOT IN (1, 2, 98, 99)) * 100.0
    / NULLIF(COUNT(*), 0),
  2) AS ptje_migrantes,

  -- Porcentaje de personas con escolaridad mayor a 16 años
  ROUND(
    COUNT(*) FILTER (WHERE p.escolaridad >= 16) * 100.0
    / NULLIF(COUNT(*) FILTER (WHERE p.escolaridad IS NOT NULL), 0),
  2) AS ptje_esc_mayor_16,

  -- Porcentaje de adultos mayores
  ROUND(
    COUNT(*) FILTER (WHERE p.p09 >= 65) * 100.0
    / NULLIF(COUNT(*) FILTER (WHERE p.p09 IS NOT NULL), 0),
  2) AS ptje_adulto_mayor

FROM public.personas   AS p
JOIN public.hogares    AS h ON p.hogar_ref_id    = h.hogar_ref_id
JOIN public.viviendas  AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas      AS z ON v.zonaloc_ref_id  = z.zonaloc_ref_id
JOIN public.comunas    AS c ON z.codigo_comuna   = c.codigo_comuna

GROUP BY z.geocodigo, c.nom_comuna
ORDER BY ptje_esc_mayor_16 DESC;
"
# Ejecutar consulta y importar resultados a data.frame en R
df_indicadores = dbGetQuery(con, sql_indicadores)

# =============================================================================
# 5) Seleccionar variables y escalarlas
# =============================================================================

vars_clusters = df_indicadores[,c("ptje_migrantes",
                                  "ptje_esc_mayor_16",
                                  "ptje_adulto_mayor")]

# Se escalan las variables
vars_scaled = scale(vars_clusters)

# 5) Método del codo para elegir K
fviz_nbclust(vars_scaled, kmeans, method = "wss") +
  labs(title = "Método del codo", x = "Número de clusters (K)", y = "WSS")

# 5) K-means
set.seed(123)
km = kmeans(vars_scaled, centers = 4, nstart = 25)

# Se incluye el número de cluster a la tabla
df_indicadores$cluster = as.factor(km$cluster)

# Escolaridad v/s Migración
ggplot(df_indicadores, aes(x = ptje_esc_mayor_12, y = ptje_migrantes, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "Escolaridad v/s Migrantes",
       x = "% Población con >= 12 años de Escolaridad",
       y = "% Población Migrante") +
  theme_minimal()

# Crear gráfico 3D
fig <- plot_ly(
  data = df_indicadores,
  x = ~ptje_migrantes,
  y = ~ptje_esc_mayor_12,
  z = ~ptje_adulto_mayor,
  color = ~as.factor(cluster),  # Asegurarse de que sea un factor para colorear por grupo
  colors = c("red", "green", "blue", "purple"), # puedes ajustar esto
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 4)
)
# Mostrar el gráfico
fig


# Seleccionar variables más el cluster como factor
df_plot = df_indicadores[, c("ptje_migrantes", "ptje_esc_mayor_16", "ptje_adulto_mayor", "cluster")]

# Gráfico de colrrelaciones
p = ggpairs(
  df_plot,
  columns = 1:3,
  mapping = aes(color = cluster), 
  upper = list(continuous = "points"),
  lower = list(continuous = "points"),
  diag  = list(continuous = "densityDiag")
)

print(p)

# Gráfico para extraer leyenda de colores
g_leyenda = ggplot(df_plot, aes(x = ptje_migrantes, y = ptje_esc_mayor_16, color = cluster)) +
  geom_point() +
  theme_minimal()

# Mostrar la leyenda 
g_leyenda

# CONSULTA DE GEOMETRÍA
sql_geometria = "
SELECT
  geocodigo::double precision AS geocodigo,
  geom
FROM dpa.zonas_censales_rm
WHERE nom_provin = 'SANTIAGO'
  AND urbano     = 1;
"

# LEER CAPA GEOGRÁFICA
sf_zonas = st_read(con, query = sql_geometria)

# COMBINAR CON INDICADORES
sf_mapa = merge(
  x     = sf_zonas,
  y     = df_indicadores,
  by    = "geocodigo",
  all.x = FALSE
)

# EXPORTAR A GEOJSON PARA USAR EN QGIS
st_write(sf_mapa, "zonas_clusters.geojson", driver = "GeoJSON", delete_dsn = TRUE)


# Se obtiene geometría comunal para Santiago
sql_comunas = "
SELECT cut, nom_comuna, geom
FROM dpa.comunas_rm_shp
WHERE nom_provin = 'SANTIAGO';
"
sf_comunas_santiago = st_read(con, query = sql_comunas)

# Calcular bounding box para limitar el mapa al área urbana de Santiago
bbox = st_bbox(sf_mapa)

# Crear mapa de clusters
mapa_clusters = ggplot() +
  geom_sf(data = sf_mapa, aes(fill = cluster), color = NA) +
  geom_sf(data = sf_comunas_santiago, fill = NA, color = "black", size = 0.4) +
  geom_sf_text(data = st_centroid(sf_comunas_santiago), aes(label = nom_comuna), size = 2, fontface = "bold") +
  scale_fill_brewer(palette = "Set2", name = "Cluster") +
  labs(
    title = "Mapa de Clusters de Zonas Censales",
    subtitle = "Provincia de Santiago, Región Metropolitana"
  ) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(mapa_clusters)


for (k in levels(df_indicadores$cluster)) {
  cat("\nCluster", k, "\n")
  grupo = sf_mapa[df_indicadores$cluster == k, ]
  print(summary(grupo[, c("ptje_migrantes", "ptje_esc_mayor_16", "ptje_adulto_mayor")]))
  cat("n =", nrow(grupo), "\n")
}
