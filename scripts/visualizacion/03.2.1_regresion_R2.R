# ============================================================
# SCRIPT: 04_analisis_alometrico.R (Versión Figura 2)
# DESCRIPCIÓN: Regresión Ancho vs Altura con R2 y paleta Q1.
# ============================================================
library(dplyr)
library(ggplot2)
library(patchwork)

# 1. Carga de datos
df <- readRDS("data/processed/fasmidos_clean.rds")

# ============================================================
# 2. DEFINICIÓN ESTRICTA DE PALETA Y FORMAS
# ============================================================
# Paleta con alto contraste perceptual
col_pap <- c(
  "No eclosionó" = "#BDBDBD",   # gris claro  → fondo/ruido
  "Eclosionó"    = "#1565C0",   # azul profundo → evento clave
  "Adulto"       = "#C62828"    # rojo          → categoría de mayor interés
)

# Formas que solo usan 'color' (sin fill interno)
shp_pap <- c(
  "No eclosionó" = 1,    # círculo vacío   ○
  "Eclosionó"    = 16,   # círculo sólido  ●
  "Adulto"       = 17    # triángulo sólido ▲
)

# ============================================================
# 3. EXTRACCIÓN ANALÍTICA DE R-CUADRADO (R2)
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
    # Coordenadas dinámicas para apilar los textos arriba a la izquierda
    x_pos = min(df$ancho, na.rm = TRUE),
    y_pos = max(df$altura, na.rm = TRUE) - (row_number() - 1) * 0.06 
  )

# ============================================================
# 4. PANEL A: Regresión Lineal Global
# ============================================================
p_global <- ggplot(df, aes(x = ancho, y = altura)) +
  # Las formas 1, 16 y 17 solo necesitan 'color', no 'fill'
  geom_point(aes(color = estado, shape = estado), alpha = 0.7, size = 2.2) +
  # Modelo global único (línea negra dominante)
  geom_smooth(method = "lm", color = "black", linewidth = 1.2, se = TRUE, fill = "grey80") +
  # Anotación del R2 Global
  annotate("text", x = min(df$ancho, na.rm = TRUE), y = max(df$altura, na.rm = TRUE), 
           label = label_global, parse = TRUE, hjust = 0, size = 4.5, fontface = "bold") +
  scale_color_manual(name = "Estado", values = col_pap) +
  scale_shape_manual(name = "Estado", values = shp_pap) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  ) +
  labs(title = "A. Regresión Morfométrica Global",
       subtitle = "Relación base Ancho vs Altura (Tendencia general)",
       x = "Ancho (w) [mm]", y = "Altura (h) [mm]")

# ============================================================
# 5. PANEL B: Regresión Estratificada
# ============================================================
p_grupos <- ggplot(df, aes(x = ancho, y = altura, color = estado, shape = estado)) +
  geom_point(alpha = 0.7, size = 2.2) +
  # El 'fill' se aplica aquí específicamente para colorear el error estándar (SE)
  geom_smooth(aes(fill = estado), method = "lm", alpha = 0.15, linewidth = 1.2, se = TRUE) +
  # Inyección de los R2 calculados
  geom_text(data = df_r2, aes(x = x_pos, y = y_pos, label = label, color = estado), 
            parse = TRUE, hjust = 0, size = 4, show.legend = FALSE, fontface = "bold", inherit.aes = FALSE) +
  scale_color_manual(name = "Estado", values = col_pap) +
  scale_fill_manual(name = "Estado", values = col_pap) + # Para las franjas de geom_smooth
  scale_shape_manual(name = "Estado", values = shp_pap) +
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
# 6. ENSAMBLE Y EXPORTACIÓN (FIGURA 2)
# ============================================================

figura_regresion <- (p_global | p_grupos) +
  plot_annotation(
    # title = "Figura 2. Análisis Alométrico: Transversalidad y Supervivencia",
    caption = "Nota: Las bandas representan el error estándar (SE). Textos muestran coeficientes de determinación (R²)."
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

print(figura_regresion)

ggsave("pics/figura_02_regresion_R2_sin_titulo.png", figura_regresion, width = 12, height = 6.5, dpi = 300)