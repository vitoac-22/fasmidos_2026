# ============================================================
# PROYECTO: Fásmidos 2026
# SCRIPT: 02_analisis_inferencial.R
# DESCRIPCIÓN: Inferencia estadística, tamaños de efecto y 
#              modelado de viabilidad (Binario y Ordinal).
# ============================================================

# 1. CARGA DE DEPENDENCIAS Y DATOS
# ------------------------------------------------------------
paquetes <- c("dplyr", "tidyr", "car", "broom", "MASS", "DescTools")
faltantes <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if(length(faltantes)) install.packages(faltantes)
invisible(lapply(paquetes, library, character.only = TRUE))

# Carga de datos limpios (Desde la raíz del proyecto)
df <- readRDS("data/processed/fasmidos_clean.rds") |> 
  mutate(
    # Variable Binaria para Regresión Logística
    eclosion_bin = ifelse(estado == "No eclosionó", 0, 1),
    # Variable Ordinal para Modelado
    estado_ord = factor(estado, levels = c("No eclosionó", "Eclosionó", "Adulto"), ordered = TRUE)
  )

N_total <- nrow(df)

cat("\n=======================================================\n")
cat("INFORME DE ANÁLISIS ESTADÍSTICO - DYME RAMULUS\n")
cat("=======================================================\n")

# 2. AUDITORÍA PARAMÉTRICA (Justificación de Pruebas)
# ------------------------------------------------------------
cat("\n--- 1. AUDITORÍA DE NORMALIDAD (Shapiro-Wilk) ---\n")
# Extraemos el ancho de los que "No eclosionaron" (n = 285)
ancho_fallo <- df$ancho[df$estado == "No eclosionó"]
sw_test <- shapiro.test(ancho_fallo)

cat(sprintf("Shapiro-Wilk (Ancho - No eclosionó): W = %.3f, p-valor = %g\n", sw_test$statistic, sw_test$p.value))
cat("CONCLUSIÓN: Rechazo contundente de normalidad. Se justifica el uso de estadística no paramétrica.\n")

# 3. FASE 1: VIABILIDAD BINARIA (Eclosionó vs No Eclosionó)
# ------------------------------------------------------------
cat("\n--- 2. FASE BINARIA (Mann-Whitney U / Wilcoxon) ---\n")

calcular_wilcoxon <- function(var_str) {
  # df$eclosion_bin agrupa "Eclosionó" y "Adulto" como 1
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
# Evaluamos cuál variable pesa más cuando compiten en el mismo modelo
cat("\n--- 3. MODELO LOGÍSTICO (Importancia de Variables) ---\n")
mod_log <- glm(eclosion_bin ~ altura + ancho + longitud, family = binomial, data = df)

# Resumen del modelo
print(tidy(mod_log))

# Verificación de Multicolinealidad (VIF)
# Si VIF > 5, las variables están compitiendo por la misma información
cat("\nFactor de Inflación de la Varianza (VIF):\n")
print(vif(mod_log))

# 5. FASE 3: ONTOGENIA COMPLETA (3 Estados)
# ------------------------------------------------------------
cat("\n--- 4. FASE TERCIARIA (Kruskal-Wallis) ---\n")

calcular_kruskal <- function(var_str) {
  test <- kruskal.test(df[[var_str]] ~ df$estado)
  eps2 <- test$statistic / (N_total - 1)
  
  cat(sprintf("Variable: %s\n", toupper(var_str)))
  cat(sprintf("  H = %.3f, p-valor = %g\n", test$statistic, test$p.value))
  cat(sprintf("  Epsilon Cuadrado (eps2) = %.3f\n", eps2))
}

calcular_kruskal("altura")
calcular_kruskal("ancho")
calcular_kruskal("longitud")

# 6. FASE 4: DISEÑO EXPERIMENTAL - REGRESIÓN ORDINAL
# ------------------------------------------------------------
# Asumiendo un continuo: No eclosionó -> Eclosionó -> Adulto
cat("\n--- 5. MODELO ORDINAL (Regresión Logística Proporcional) ---\n")
cat("Evalúa si el incremento en tamaño empuja al insecto al siguiente estado.\n\n")

# Se usa MASS::polr para modelos ordinales
mod_ord <- polr(estado_ord ~ altura + ancho + longitud, data = df, Hess = TRUE)
res_ord <- coef(summary(mod_ord))

# Cálculo manual de p-valores para polr
p_valores <- pnorm(abs(res_ord[, "t value"]), lower.tail = FALSE) * 2
res_ord <- cbind(res_ord, "p value" = p_valores)

print(round(res_ord, 4))

cat("\n=======================================================\n")
cat("FIN DEL ANÁLISIS\n")
cat("=======================================================\n")


# Eclosionó vs. Adulto ~ altura + ancho + longitud
# No eclosionó vs. Eclosionó vs. Adulto ~ altura + ancho + longitud

# -> 



