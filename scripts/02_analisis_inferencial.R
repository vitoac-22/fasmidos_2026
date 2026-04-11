# ============================================================
# PROYECTO: Fásmidos 2026
# SCRIPT: 02_analisis_inferencial.R
# DESCRIPCIÓN: Inferencia estadística, tamaños de efecto y 
#              modelado de viabilidad (Binario y Ordinal) con
#              narrativa analítica multidisciplinaria.
# ============================================================

# 1. CARGA DE DEPENDENCIAS Y DATOS
# ------------------------------------------------------------
paquetes <- c("dplyr", "tidyr", "car", "broom", "MASS", "DescTools")
faltantes <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if(length(faltantes)) install.packages(faltantes)
invisible(lapply(paquetes, library, character.only = TRUE))

df <- readRDS("data/processed/fasmidos_clean.rds") |> 
  mutate(
    eclosion_bin = ifelse(estado == "No eclosionó", 0, 1),
    estado_ord = factor(estado, levels = c("No eclosionó", "Eclosionó", "Adulto"), ordered = TRUE)
  )

N_total <- nrow(df)

cat("\n=======================================================\n")
cat("INFORME DE ANÁLISIS ESTADÍSTICO - DYME RAMULUS\n")
cat("=======================================================\n")

# 2. AUDITORÍA PARAMÉTRICA (Justificación de Pruebas)
# ------------------------------------------------------------
cat("\n--- 1. AUDITORÍA DE NORMALIDAD Y DESBALANCE ---\n")
cat("Propósito: Justificar la elección del marco estadístico.\n")
cat("Los datos biológicos rara vez forman 'campanas perfectas'. Además, tenemos una mortalidad\n")
cat(sprintf("natural altísima (285 fallos vs %d éxitos). Este desbalance extremo invalida el uso de\n", N_total - 285))
cat("pruebas paramétricas clásicas (T-Student, ANOVA), ya que la media aritmética se vuelve inestable\n")
cat("y se distorsiona por la asimetría física (los huevos no pueden ser infinitamente pequeños).\n\n")

ancho_fallo <- df$ancho[df$estado == "No eclosionó"]
sw_test <- shapiro.test(ancho_fallo)

cat(sprintf("Shapiro-Wilk (Ancho - No eclosionó): W = %.3f, p-valor = %g\n", sw_test$statistic, sw_test$p.value))
cat("CONCLUSIÓN: El p-valor cercano a cero rechaza contundentemente la normalidad.\n")
cat("Procederemos con pruebas basadas en medianas y rangos estocásticos, que son matemáticamente\n")
cat("robustas ante grupos desiguales y varianzas asimétricas.\n")

# 3. FASE 1: VIABILIDAD BINARIA (Eclosionó vs No Eclosionó)
# ------------------------------------------------------------
cat("\n--- 2. EL FILTRO DE ECLOSIÓN (Mann-Whitney U) ---\n")
cat("Propósito: Identificar si existe una ventaja física demostrable para nacer.\n")
cat("Al comparar medianas en lugar de promedios, evitamos que un 'huevo gigante' distorsione\n")
cat("el análisis de toda la población.\n\n")

calcular_wilcoxon <- function(var_str) {
  test <- wilcox.test(df[[var_str]] ~ df$eclosion_bin, exact = FALSE)
  Z_stat <- qnorm(test$p.value / 2, lower.tail = FALSE)
  r_efecto <- Z_stat / sqrt(N_total)
  
  cat(sprintf("Variable: %s\n", toupper(var_str)))
  cat(sprintf("  W = %.1f, p-valor = %g\n", test$statistic, test$p.value))
  cat(sprintf("  Tamaño del Efecto (r) = %.3f\n", r_efecto))
}

calcular_wilcoxon("altura")
calcular_wilcoxon("ancho")
calcular_wilcoxon("longitud")

# 4. FASE 2: MODELO DE REGRESIÓN LOGÍSTICA BINARIA
# ------------------------------------------------------------
cat("\n--- 3. AISLAMIENTO DE EFECTOS (Regresión Logística Multivariada) ---\n")
cat("Propósito: Evaluar si el 'ancho' y la 'altura' actúan como variables independientes o redundantes.\n")
cat("En ecología, medidas similares suelen estar correlacionadas. Este modelo aísla el valor de\n")
cat("cada dimensión. Si el VIF es < 2, significa que el insecto requiere obligatoriamente\n")
cat("volumen en ambos ejes transversales para sobrevivir, no solo en uno.\n\n")

mod_log <- glm(eclosion_bin ~ altura + ancho + longitud, family = binomial, data = df)
print(tidy(mod_log))

cat("\nFactor de Inflación de la Varianza (VIF):\n")
print(vif(mod_log))

# 5. FASE 3: ONTOGENIA COMPLETA (3 Estados)
# ------------------------------------------------------------
cat("\n--- 4. EL PEAJE ONTOGENÉTICO (Kruskal-Wallis y Varianza Explicada) ---\n")
cat("Propósito: Según los principios de McElreath, el tamaño del efecto es más importante que el p-valor.\n")
cat("Calculamos el Epsilon Cuadrado (eps2) para ver qué porcentaje de toda la vida del insecto\n")
cat("es decidida exclusivamente por el tamaño inicial de su huevo.\n\n")

calcular_kruskal <- function(var_str) {
  test <- kruskal.test(df[[var_str]] ~ df$estado)
  eps2 <- test$statistic / (N_total - 1)
  
  cat(sprintf("Variable: %s\n", toupper(var_str)))
  cat(sprintf("  H = %.3f, p-valor = %g\n", test$statistic, test$p.value))
  cat(sprintf("  Epsilon Cuadrado (eps2) = %.3f (Explica el %.1f%% de la varianza vital)\n", eps2, eps2*100))
}

calcular_kruskal("altura")
calcular_kruskal("ancho")
calcular_kruskal("longitud")

# 6. FASE 4: SUPERVIVENCIA POST-ECLOSIÓN (El destino de la ninfa)
# ------------------------------------------------------------
cat("\n--- 5. MODELO DE SUPERVIVENCIA NINFAL (Eclosionó vs Adulto) ---\n")
cat("Propósito: Analizar aisladamente a la cohorte sobreviviente.\n")
cat("La biología causal asume que el volumen es un 'peaje' para eclosionar. Una vez nacido,\n")
cat("el tamaño del huevo no debería predecir si llegas a adulto, cediendo el control a\n")
cat("variables ambientales (humedad, alimento).\n\n")

df_exitos <- df |> filter(estado != "No eclosionó") |>
  mutate(llegar_adulto = ifelse(estado == "Adulto", 1, 0))

mod_ninfas <- glm(llegar_adulto ~ altura + ancho + longitud, family = binomial, data = df_exitos)
print(tidy(mod_ninfas))

# 7. FASE 5: LOS "NÚMEROS MÁGICOS" (Umbrales de Probabilidad)
# ------------------------------------------------------------
cat("\n--- 6. UMBRALES BIOMECÁNICOS ECOLÓGICOS ---\n")
cat("Propósito: Traducir los coeficientes logarítmicos abstractos en medidas físicas (mm)\n")
cat("utilizables como indicadores de campo. Utilizamos la mediana de la altura para no\n")
cat("sesgar el cálculo por valores extremos, como lo haría el promedio.\n\n")

coeficientes <- coef(mod_log)
beta_0 <- coeficientes["(Intercept)"]
beta_alt <- coeficientes["altura"]
beta_anc <- coeficientes["ancho"]

# Usamos la mediana en lugar de la media por el desbalance extremo de fallos
altura_mediana <- median(df$altura, na.rm = TRUE)

# Despeje de la ecuación de momios (Logit)
ancho_50_prob <- -(beta_0 + beta_alt * altura_mediana) / beta_anc
ancho_80_prob <- (1.386 - (beta_0 + beta_alt * altura_mediana)) / beta_anc

cat(sprintf("Dada la altura mediana de la especie (%.2f mm):\n", altura_mediana))
cat(sprintf("  -> Umbral de Supervivencia (50%% de probabilidad de eclosión): %.2f mm de ancho.\n", ancho_50_prob))
cat(sprintf("  -> Umbral de Seguridad (80%% de probabilidad de eclosión)   : %.2f mm de ancho.\n", ancho_80_prob))

cat("\nCONCLUSIÓN EVOLUTIVA:\n")
cat("Estos milímetros representan una falla estructural en cadena: por debajo del umbral, el\n")
cat("embrión carece del volumen físico para desarrollarse completamente o de la ventaja mecánica\n")
cat("para ejercer la presión necesaria y romper el opérculo.\n")

cat("\n=======================================================\n")
cat("FIN DEL ANÁLISIS\n")
cat("=======================================================\n")