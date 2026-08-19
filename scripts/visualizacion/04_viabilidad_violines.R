# ============================================================
# SCRIPT: 04_viabilidad_violines.R
# DESCRIPCIÓN: Distribuciones univariadas y violines (Gráfica pura).
# ============================================================
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. PALETA ESTRICTA
col_pap <- c("No eclosionó" = "#BDBDBD", "Eclosionó" = "#1565C0", "Adulto" = "#C62828")
shp_pap <- c("No eclosionó" = 1, "Eclosionó" = 16, "Adulto" = 17)

# 2. INGESTA Y PREPARACIÓN
df <- readRDS("data/processed/fasmidos_clean.rds")
df_binario <- df |>
  mutate(exito_eclosion = factor(ifelse(estado == "No eclosionó", "No eclosionó", "Eclosionó"), 
                                 levels = c("No eclosionó", "Eclosionó")))

df_long <- df_binario |>
  select(exito_eclosion, ancho, altura, longitud) |>
  pivot_longer(cols = c(ancho, altura, longitud), names_to = "dimension", values_to = "medida") |>
  mutate(dimension = factor(dimension, levels = c("ancho", "altura", "longitud"),
                            labels = c("Ancho (w) ***", "Altura (h) ***", "Longitud (l) - NS")))

# 3. CONSTRUCCIÓN DE GRÁFICA
figura_paper <- ggplot(df_long, aes(x = exito_eclosion, y = medida, fill = exito_eclosion, color = exito_eclosion)) +
  geom_violin(trim = FALSE, alpha = 0.4, color = "grey30", linewidth = 0.5) +
  geom_jitter(aes(shape = exito_eclosion), width = 0.15, alpha = 0.8, size = 1.8) +
  geom_boxplot(width = 0.12, fill = "white", color = "black", outlier.shape = NA, linewidth = 0.6, alpha = 0.7) +
  facet_wrap(~dimension, scales = "free_y") +
  scale_fill_manual(values = col_pap) + scale_color_manual(values = col_pap) + scale_shape_manual(values = shp_pap) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 14),
        strip.background = element_rect(fill = "grey95", color = "black", linewidth = 1),
        strip.text = element_text(face = "bold", size = 11), axis.text.x = element_text(face = "bold")) +
  labs(title = "Figura 2. Determinantes Morfológicos de Viabilidad",
       subtitle = "Comparación de dimensiones transversales y longitudinales",
       caption = "Prueba de Wilcoxon (Mann-Whitney U). *** p < 0.001, NS (No Significativo).",
       x = "Destino del Individuo", y = "Dimensión Física [mm]")

print(figura_paper)
ggsave("figura_02_viabilidad_violines.jpg", plot = figura_paper, path = "pics", width = 8.5, height = 5.5, dpi = 300)