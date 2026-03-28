# ============================================================
# 0. CARGA DE DEPENDENCIAS
# ============================================================
paquetes <- c("dplyr", "readxl", "glue")
faltantes <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if(length(faltantes)) install.packages(faltantes)
invisible(lapply(paquetes, library, character.only = TRUE))

# ============================================================
# 1. CARGA Y ARQUITECTURA DE DATOS
# ============================================================
df_raw <- read_excel("data/p_EGGS_EJES.xlsx", sheet = "DATA_MOD_01")

df <- df_raw %>%
  mutate(
    # FASE 2: La variable triple (Estado Final)
    estado_triple = case_when(
      estado == "Adulto" & eclosion == "SI" & vivos == "NO" ~ "Eclosionó",
      estado == "Eclosion" & eclosion == "SI" & vivos == "SI" ~ "Adulto",
      estado == "Eclosion" ~ "Eclosionó",
      estado == "No viable" ~ "No eclosionó",
      TRUE ~ estado
    ),
    estado_triple = factor(estado_triple, levels = c("No eclosionó", "Eclosionó", "Adulto")),
    
    # FASE 1: La variable binaria de viabilidad temprana (Eclosionó + Adulto = Éxito)
    estado_binario = factor(ifelse(estado_triple == "No eclosionó", "No eclosionó", "Éxito"), 
                            levels = c("No eclosionó", "Éxito"))
  )

N_total <- nrow(df)

# ============================================================
# 2. MOTORES ESTADÍSTICOS Y TAMAÑO DE EFECTO (La Relevancia)
# ============================================================

# Función matemática para Fase 1 (Wilcoxon + Efecto r)
calc_binario <- function(var_name) {
  test <- wilcox.test(df[[var_name]] ~ df$estado_binario, exact = FALSE)
  # Cálculo manual de Z a partir del p-valor para obtener 'r'
  Z_stat <- qnorm(test$p.value / 2, lower.tail = FALSE)
  r_efecto <- Z_stat / sqrt(N_total)
  list(W = test$statistic, p = test$p.value, r = r_efecto)
}

# Función matemática para Fase 2 (Kruskal-Wallis + Efecto Epsilon Cuadrado)
calc_triple <- function(var_name) {
  test <- kruskal.test(df[[var_name]] ~ df$estado_triple)
  H_stat <- test$statistic
  # Epsilon cuadrado de Tomczak & Tomczak (2014)
  eps_cuadrado <- H_stat / (N_total - 1)
  list(H = H_stat, p = test$p.value, eps2 = eps_cuadrado)
}

# Ejecución
b_alt <- calc_binario("altura")
b_lon <- calc_binario("longitud")
b_anc <- calc_binario("ancho")

t_alt <- calc_triple("altura")
t_lon <- calc_triple("longitud")
t_anc <- calc_triple("ancho")

# Formateador riguroso
fp <- function(p) ifelse(p < 0.001, "< 0.001", sprintf("= %.3f", p))
fr <- function(r) sprintf("%.3f", r)

# ============================================================
# 3. GENERACIÓN DEL MANUSCRITO (Automatizado con Glue)
# ============================================================

texto_paper <- glue("
  -------------------------------------------------------------------------
  [SECCIÓN DE RESULTADOS: EFECTOS MORFOMÉTRICOS SOBRE LA SUPERVIVENCIA]
  -------------------------------------------------------------------------
  
  Fase 1: Viabilidad de Eclosión (Análisis Binario)
  Para determinar el impacto morfológico en la viabilidad embrionaria temprana, se evaluó el éxito de eclosión global. Las pruebas de suma de rangos de Wilcoxon revelaron que la sección transversal del huevo es un determinante crítico. Se observaron diferencias altamente significativas y tamaños de efecto de relevancia moderada-alta en el ancho (W = {b_anc$W}, p {fp(b_anc$p)}, r = {fr(b_anc$r)}) y la altura (W = {b_alt$W}, p {fp(b_alt$p)}, r = {fr(b_alt$r)}). Por el contrario, la longitud del huevo demostró ser irrelevante biológicamente para la viabilidad temprana (W = {b_lon$W}, p {fp(b_lon$p)}, r = {fr(b_lon$r)}), indicando que un eje longitudinal mayor no confiere ventajas probabilísticas de eclosión.

  Fase 2: Ontogenia Completa (Análisis Terciario)
  Para hilar más fino y determinar si la morfología inicial condiciona la supervivencia hasta la madurez, se segregó la muestra en tres destinos ontogenéticos: fallo embrionario ('No eclosionó'), mortalidad ninfal ('Eclosionó') y éxito total ('Adulto'). La prueba de Kruskal-Wallis corroboró la significancia estructural del ancho (H = {fr(t_anc$H)}, p {fp(t_anc$p)}, \u03B5\u00B2 = {fr(t_anc$eps2)}) y la altura (H = {fr(t_alt$H)}, p {fp(t_alt$p)}, \u03B5\u00B2 = {fr(t_alt$eps2)}). Sin embargo, los bajos valores del tamaño de efecto Epsilon Cuadrado (\u03B5\u00B2) sugieren que, si bien la morfología transversal actúa como un filtro biológico primario para la eclosión, su capacidad predictiva o 'relevancia' decae severamente para predecir si una ninfa alcanzará la etapa adulta. La longitud continuó siendo un predictor nulo a lo largo de toda la ontogenia (H = {fr(t_lon$H)}, p {fp(t_lon$p)}, \u03B5\u00B2 = {fr(t_lon$eps2)}).
  -------------------------------------------------------------------------
")

cat(texto_paper)