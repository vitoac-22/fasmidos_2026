# ============================================================
# ANÁLISIS ALOMÉTRICO: Ancho vs Altura (Con R2 y Simetría Visual)
# ============================================================
library(dplyr)
library(ggplot2)
library(patchwork)

# 1. Carga de datos
df <- readRDS("data/processed/fasmidos_clean.rds")

# Paletas estrictas y unificadas
col_tri <- c("No eclosionó" = "#9E9E9E", "Eclosionó" = "#457b9d", "Adulto" = "#2E7D32")
shp_tri <- c("No eclosionó" = 4, "Eclosionó" = 21, "Adulto" = 24)

# ============================================================
# 2. EXTRACCIÓN ANALÍTICA DE R-CUADRADO (R2)
# ============================================================
# R2 Global
mod_global <- lm(altura ~ ancho, data = df)
r2_global_val <- summary(mod_global)$r.squared
label_global <- sprintf("italic(R)^2 == %.3f", r2_global_val)

# R2 Estratificado por Estado Biológico
df_r2 <- df %>%
  group_by(estado) %>%
  summarise(
    r2_val = summary(lm(altura ~ ancho))$r.squared,
    .groups = 'drop'
  ) %>%
  mutate(
    # Generamos la etiqueta matemática
    label = sprintf("italic(R)^2 == %.3f", r2_val),
    # Calculamos coordenadas dinámicas para apilar los textos arriba a la izquierda
    x_pos = min(df$ancho),
    y_pos = max(df$altura) - (row_number() - 1) * 0.06 
  )

# ============================================================
# 3. GRÁFICA A: Regresión Lineal Global
# ============================================================
p_global <- ggplot(df, aes(x = ancho, y = altura)) +
  # Inyección estricta de color, relleno y forma SOLO a los puntos
  geom_point(aes(color = estado, fill = estado, shape = estado), alpha = 0.6, size = 2.5) +
  # Modelo global único (línea negra dominante)
  geom_smooth(method = "lm", color = "black", linewidth = 1.2, se = TRUE, fill = "grey80") +
  # Anotación del R2 Global
  annotate("text", x = min(df$ancho), y = max(df$altura), label = label_global, 
           parse = TRUE, hjust = 0, size = 4.5, fontface = "bold") +
  # Sincronización de escalas
  scale_color_manual(name = "Estado", values = col_tri) +
  scale_fill_manual(name = "Estado", values = col_tri) +
  scale_shape_manual(name = "Estado", values = shp_tri) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  ) +
  labs(title = "A. Regresión Morfométrica Global",
       subtitle = "Relación base Ancho vs Altura (Tendencia de la especie)",
       x = "Ancho (w) [mm]", y = "Altura (h) [mm]")

# ============================================================
# 4. GRÁFICA B: Regresión Estratificada
# ============================================================
p_grupos <- ggplot(df, aes(x = ancho, y = altura, color = estado, fill = estado, shape = estado)) +
  geom_point(alpha = 0.6, size = 2.5) +
  # Modelos divididos por grupo
  geom_smooth(method = "lm", alpha = 0.15, linewidth = 1.2, se = TRUE) +
  # Inyección de los R2 calculados (heredan el color automáticamente)
  geom_text(data = df_r2, aes(x = x_pos, y = y_pos, label = label, color = estado), 
            parse = TRUE, hjust = 0, size = 4, show.legend = FALSE, fontface = "bold", inherit.aes = FALSE) +
  scale_color_manual(name = "Estado", values = col_tri) +
  scale_fill_manual(name = "Estado", values = col_tri) +
  scale_shape_manual(name = "Estado", values = shp_tri) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  ) +
  labs(title = "B. Regresión Estratificada por Viabilidad",
       subtitle = "Descomposición de trayectorias alométricas",
       x = "Ancho (w) [mm]", y = "Altura (h) [mm]")

# ============================================================
# 5. ENSAMBLE Y EXPORTACIÓN
# ============================================================

figura_regresion <- (p_global | p_grupos) +
  plot_annotation(
    caption = "Nota: Las bandas de sombra representan el error estándar (SE). Textos muestran coeficientes de determinación (R²)."
  )

print(figura_regresion)
# ggsave("pics/figura_5_regresion_R2.png", figura_regresion, width = 12, height = 5.5, dpi = 300)