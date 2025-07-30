# 1.a) Junta todos los cons_i en uno solo
cons_region <- do.call(rbind, cons_censo_comunas)

# 1.b) Prepara la tabla de individuos de Casen a nivel regional
inds_region <- casen_pob %>%
  mutate(
    Comuna = factor(Comuna),               # la comuna como factor
    edad_cat      = edad_cat,              # ya vienen recodificadas
    esc_cat       = esc_cat,
    sexo_cat      = sexo_cat
  ) %>%
  select(ID, Comuna, edad_cat, esc_cat, sexo_cat)

# Renombra para que casen con los nombres de cons_region
names(inds_region) <- c("ID","Comuna","Edad","Escolaridad","Sexo")

sim_i <- integerise(
  weights = w_frac,
  inds     = inds_i,
  method   = "probabilistic",  # o "prob", o el nombre que acepte tu paquete
  seed     = 123
)
