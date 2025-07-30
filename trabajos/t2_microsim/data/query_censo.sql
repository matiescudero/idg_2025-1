-- data/query_censo.sql

SELECT
  z.geocodigo AS "GEOCODIGO",

  -- Variables de edad (toda la población)
  COUNT(*) FILTER (WHERE p.p09 < 30)                     AS "edad_0_30",
  COUNT(*) FILTER (WHERE p.p09 >= 30 AND p.p09 < 40)     AS "edad_30_40",
  COUNT(*) FILTER (WHERE p.p09 >= 40 AND p.p09 < 50)     AS "edad_40_50",
  COUNT(*) FILTER (WHERE p.p09 >= 50 AND p.p09 < 60)     AS "edad_50_60",
  COUNT(*) FILTER (WHERE p.p09 >= 60 AND p.p09 < 70)     AS "edad_60_70",
  COUNT(*) FILTER (WHERE p.p09 >= 70 AND p.p09 < 80)     AS "edad_70_80",
  COUNT(*) FILTER (WHERE p.p09 >= 80)                    AS "edad_mayor_80",

  -- Variables de escolaridad (toda la población)
  COUNT(*) FILTER (WHERE p.escolaridad = 0)              AS "esco_0",
  COUNT(*) FILTER (WHERE p.escolaridad > 0  AND p.escolaridad <= 8)   AS "esco_1_8",
  COUNT(*) FILTER (WHERE p.escolaridad >= 9 AND p.escolaridad <= 12)  AS "esco_8_12",
  COUNT(*) FILTER (WHERE p.escolaridad > 12)             AS "esco_mayor_12",

  -- Variables de sexo (toda la población)
  COUNT(*) FILTER (WHERE p.p08 = 2)                      AS "sexo_f",
  COUNT(*) FILTER (WHERE p.p08 = 1)                      AS "sexo_m",

  -- Código de comuna
  c.codigo_comuna                                       AS "COMUNA"

FROM
  personas p
  JOIN hogares     h  ON p.hogar_ref_id     = h.hogar_ref_id
  JOIN viviendas   v  ON h.vivienda_ref_id  = v.vivienda_ref_id
  JOIN zonas       z  ON v.zonaloc_ref_id   = z.zonaloc_ref_id
  JOIN comunas     c  ON z.codigo_comuna    = c.redcoden
  JOIN provincias  pr ON c.provincia_ref_id = pr.provincia_ref_id

WHERE
     pr.nom_provincia = 'SANTIAGO'
  OR c.nom_comuna    IN ('PUENTE ALTO','SAN BERNARDO')

GROUP BY
  z.geocodigo,
  c.codigo_comuna

ORDER BY
  z.geocodigo;
