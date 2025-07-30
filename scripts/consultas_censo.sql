SELECT
  z.geocodigo AS geocodigo,
  
  -- tus rangos de edad…
  SUM(CASE WHEN p.p09 < 30                   THEN 1 ELSE 0 END) AS edad_menor_30,
  /* … */

  -- tus rangos de escolaridad…
  SUM(CASE WHEN p.escolaridad = 0                                                     THEN 1 ELSE 0 END) AS esco_0,
  /* … */
  SUM(CASE WHEN p.escolaridad > 12 AND p.escolaridad NOT IN (27,99)                  THEN 1 ELSE 0 END) AS esco_mayor_12,

  -- nuevos conteos puntuales
  SUM(CASE WHEN p.escolaridad = 27 THEN 1 ELSE 0 END) AS esco_27,
  SUM(CASE WHEN p.escolaridad = 99 THEN 1 ELSE 0 END) AS esco_99,

  -- sexo “limpio”
  SUM(CASE WHEN p.p08 = 1 THEN 1 ELSE 0 END) AS sexo_m,
  SUM(CASE WHEN p.p08 = 2 THEN 1 ELSE 0 END) AS sexo_f,
  -- casos “otros” o inválidos
  SUM(CASE WHEN p.p08 NOT IN (1,2) THEN 1 ELSE 0 END) AS sexo_otro,

  c.codigo_comuna AS comuna

FROM
  personas p
  JOIN hogares     h ON p.hogar_ref_id     = h.hogar_ref_id
  JOIN viviendas   v ON h.vivienda_ref_id  = v.vivienda_ref_id
  JOIN zonas       z ON v.zonaloc_ref_id   = z.zonaloc_ref_id
  JOIN comunas     c ON z.codigo_comuna     = c.redcoden
  JOIN provincias  pr ON c.provincia_ref_id = pr.provincia_ref_id

WHERE
     pr.nom_provincia = 'SANTIAGO'
  OR c.nom_comuna    IN ('PUENTE ALTO','SAN BERNARDO')

GROUP BY
  z.geocodigo,
  c.codigo_comuna

ORDER BY
  z.geocodigo;





SELECT
  COUNT(*) AS total_personas,

  -- p08 inválido (ni 1 ni 2)
  SUM( (p.p08 NOT IN (1,2))::int ) AS total_p08_invalid,
  ROUND(100.0 * SUM( (p.p08 NOT IN (1,2))::int ) / COUNT(*), 2) 
    AS pct_p08_invalid,

  -- escolaridad especiales
  SUM( (p.escolaridad = 27)::int ) AS total_esco_27,
  ROUND(100.0 * SUM( (p.escolaridad = 27)::int ) / COUNT(*), 2) 
    AS pct_esco_27,

  SUM( (p.escolaridad = 99)::int ) AS total_esco_99,
  ROUND(100.0 * SUM( (p.escolaridad = 99)::int ) / COUNT(*), 2) 
    AS pct_esco_99,

  -- p15: curso más alto aprobado
  SUM( (p.p15 = 98)::int ) AS total_p15_98,   -- valor perdido
  ROUND(100.0 * SUM( (p.p15 = 98)::int ) / COUNT(*), 2)
    AS pct_p15_98,

  SUM( (p.p15 = 99)::int ) AS total_p15_99,   -- no aplica
  ROUND(100.0 * SUM( (p.p15 = 99)::int ) / COUNT(*), 2)
    AS pct_p15_99

FROM personas p;
