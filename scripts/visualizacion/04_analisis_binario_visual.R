# ============================================================
# PROYECTO: Fásmidos 2026
# SCRIPT: 04_analisis_binario_visual.R
# DESCRIPCIÓN: Cálculo de métricas estocásticas y generación 
#              de distribuciones univariadas (Sin interpretaciones).
# ============================================================

# 1. CARGA DE DEPENDENCIAS
# ------------------------------------------------------------
paquetes <- c("dplyr", "tidyr", "ggplot2", "readxl", "glue")
faltantes <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if(length(faltantes)) install.packages(faltantes)
invisible(lapply(paquetes, library, character.only = TRUE))

# 2. EXTRACCIÓN Y LIMPIEZA BINARIA
# ------------------------------------------------------------
df <- read_excel("data/raw/p_EGGS_EJES.xlsx", sheet = "DATA_MOD_01")

df_binario <- df |> 
  mutate(
    exito_eclosion = factor(
      ifelse(estado == "No viable", "No eclosionó", "Eclosionó"), 
      levels = c("No eclosionó", "Eclosionó")
    )
  )

N_total <- nrow(df_binario)

# 3. PRUEBAS MATEMÁTICAS ESTRICTAS
# ------------------------------------------------------------
ancho_fallo <- df_binario$ancho[df_binario$exito_eclosion == "No eclosionó"]
p_shapiro_fallo <- shapiro.test(ancho_fallo)$p.value

test_ancho <- wilcox.test(ancho ~ exito_eclosion, data = df_binario, exact = FALSE)
test_altura <- wilcox.test(altura ~ exito_eclosion, data = df_binario, exact = FALSE)
test_longitud <- wilcox.test(longitud ~ exito_eclosion, data = df_binario, exact = FALSE)

formatear_p <- function(p) {
  if (p < 0.001) return("< 0.001")
  return(sprintf("= %.3f", p))
}

# 4. REPORTE ESTADÍSTICO NEUTRAL (Salida en Consola)
# ------------------------------------------------------------
texto_informe <- glue::glue("
============================================================
REPORTE ESTADÍSTICO CRUDO - VIABILIDAD BINARIA (N = {N_total})
============================================================
[1] PRUEBA DE NORMALIDAD (Shapiro-Wilk)
    - Ancho (Clase mayoritaria): p {formatear_p(p_shapiro_fallo)}

[2] PRUEBA DE HIPÓTESIS (Mann-Whitney U)
    - Dimensión Ancho    : W = {test_ancho$statistic}, p {formatear_p(test_ancho$p.value)}
    - Dimensión Altura   : W = {test_altura$statistic}, p {formatear_p(test_altura$p.value)}
    - Dimensión Longitud : W = {test_longitud$statistic}, p {formatear_p(test_longitud$p.value)}
============================================================
")

cat("\n", texto_informe, "\n")

# 5. GRÁFICA DE DISTRIBUCIÓN
# ------------------------------------------------------------
df_long <- df_binario |> 
  select(exito_eclosion, ancho, altura, longitud) |> 
  pivot_longer(cols = c(ancho, altura, longitud), names_to = "dimension", values_to = "medida") |> 
  mutate(
    dimension = factor(dimension, 
                       levels = c("ancho", "altura", "longitud"),
                       labels = c("Ancho (w) ***", "Altura (h) ***", "Longitud (l) - NS"))
  )

figura_paper <- ggplot(df_long, aes(x = exito_eclosion, y = medida, fill = exito_eclosion)) +
  
  geom_violin(trim = FALSE, alpha = 0.75, color = "grey30", linewidth = 0.5) +
  
  geom_boxplot(width = 0.12, fill = "white", color = "black", 
               outlier.shape = 21, outlier.fill = "black", outlier.size = 1.2, linewidth = 0.6) +
  
  facet_wrap(~dimension, scales = "free_y") +
  
  scale_fill_manual(values = c("No eclosionó" = "#e07a5f", "Eclosionó" = "#81b29a")) +
  
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 5), hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "grey30", margin = margin(b = 15), hjust = 0.5),
    plot.caption = element_text(size = 9, color = "grey50", hjust = 0, margin = margin(t = 10)),
    strip.background = element_rect(fill = "grey95", color = "black", linewidth = 1),
    strip.text = element_text(face = "bold", size = 11),
    axis.text.x = element_text(color = "black", face = "bold", size = 11),
    axis.text.y = element_text(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  labs(
    title = "Distribución Morfológica según Viabilidad",
    subtitle = "Comparación de dimensiones físicas por destino de eclosión",
    caption = "Prueba de Wilcoxon (Mann-Whitney U). Nivel de significancia: *** p < 0.001, NS (No Significativo).",
    x = "Destino del Individuo", 
    y = "Dimensión Física [mm]"
  )

print(figura_paper)

# Exportación en JPG para evitar errores previos con el motor TIFF/LZW
ggsave("figura_3_final_paper.jpg", plot = figura_paper, path = "pics", width = 8.5, height = 5.5, dpi = 300)