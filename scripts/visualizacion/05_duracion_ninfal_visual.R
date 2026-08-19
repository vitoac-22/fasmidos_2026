# ============================================================
# SCRIPT: duracion_ontogenetica_dual.R
# DESCRIPCIÓN: Visualización de tendencia central (SD) superpuesta 
#              con rangos absolutos (Min-Max).
# ============================================================

library(dplyr)
library(ggplot2)

# 1. INGESTA DE DATOS CON RANGOS ABSOLUTOS
# ------------------------------------------------------------
df_ninfas <- data.frame(
  estadio = factor(
    c("Instar I", "Instar II", "Instar III", "Instar IV", "Instar V", "Instar VI"),
    levels = c("Instar I", "Instar II", "Instar III", "Instar IV", "Instar V", "Instar VI")
  ),
  media_dias = c(24.7, 21.41, 23.1, 21.2, 22.2, 23.2),
  sd = c(1.3, 1.8, 2.5, 6.4, 2.6, 1.4),
  rango_min = c(21, 15, 13, 14, 16, 19),
  rango_max = c(29, 31, 33, 32, 28, 28)
)

# 2. TRANSFORMACIÓN GEOMÉTRICA
# ------------------------------------------------------------
df_plot <- df_ninfas |>
  mutate(
    ymin_sd = media_dias - sd,
    ymax_sd = media_dias + sd,
    x_num = as.numeric(estadio) 
  )

# 3. CONSTRUCCIÓN DE LA GRÁFICA MULTICAPA
# ------------------------------------------------------------
p_duracion_dual <- ggplot(df_plot, aes(x = x_num, y = media_dias)) +
  
  # CAPA 1: Rangos Absolutos (Líneas verticales grises)
  geom_linerange(aes(ymin = rango_min, ymax = rango_max), 
                 color = "grey70", linewidth = 1.5, alpha = 0.6) +
  
  # CAPA 2: Desviación Estándar (Franja principal)
  geom_ribbon(aes(ymin = ymin_sd, ymax = ymax_sd), 
              fill = "#F2E2D2", alpha = 0.8) +
  
  # CAPA 3: Tendencia Central y Nodos
  geom_line(color = "#008B6B", linewidth = 1.2) +
  geom_point(color = "#008B6B", size = 3.5) +
  
  # Anotaciones
  geom_text(aes(label = round(media_dias, 1)), 
            vjust = -2, color = "#008B6B", fontface = "bold", size = 4) +
  
  # Ejes y Estética
  scale_x_continuous(breaks = 1:6, labels = df_plot$estadio) +
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(color = "black", margin = margin(t = 5)),
    axis.text.y = element_text(color = "black"),
    panel.grid.major.y = element_line(color = "grey90", linetype = "dotted"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 11, margin = margin(b = 15))
  ) +
  labs(
    title = "Trayectoria Temporal del Desarrollo Ninfal",
    subtitle = "Franja: Desviación Estándar (± 1 SD) | Barras verticales: Rango absoluto (Min-Max)",
    x = "Estadio Ninfal",
    y = "Duración (días)"
  )

print(p_duracion_dual)

# 4. EXPORTACIÓN
# ------------------------------------------------------------
ggsave("pics/curva_duracion_ninfal_multicapa.jpg", plot = p_duracion_dual, width = 9, height = 5.5, dpi = 300)