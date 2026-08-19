# ============================================================
# SCRIPT: 06_alometria_visual.R
# DESCRIPCIÓN: Regresión Ancho vs Altura (Con R2 y Simetría Visual)
# ============================================================
library(dplyr)
library(ggplot2)
library(patchwork)

df <- readRDS("data/processed/fasmidos_clean.rds")

# 1. PALETAS ESTRICTAS Y UNIFICADAS
col_pap <- c("No eclosionó" = "#BDBDBD", "Eclosionó" = "#1565C0", "Adulto" = "#C62828")
shp_pap <- c("No eclosionó" = 1, "Eclosionó" = 16, "Adulto" = 17)

# 2. EXTRACCIÓN ANALÍTICA DE R-CUADRADO (R2)
mod_global <- lm(altura ~ ancho, data = df)
label_global <- sprintf("italic(R)^2 == %.3f", summary(mod_global)$r.squared)

df_r2 <- df |>
  group_by(estado) |>
  summarise(r2_val = summary(lm(altura ~ ancho))$r.squared, .groups = 'drop') |>
  mutate(label = sprintf("italic(R)^2 == %.3f", r2_val),
         x_pos = min(df$ancho, na.rm = TRUE),
         y_pos = max(df$altura, na.rm = TRUE) - (row_number() - 1) * 0.06)

# 3. GRÁFICA A: Regresión Lineal Global
p_global <- ggplot(df, aes(x = ancho, y = altura)) +
  geom_point(aes(color = estado, shape = estado), alpha = 0.7, size = 2.2) +
  geom_smooth(method = "lm", color = "black", linewidth = 1.2, se = TRUE, fill = "grey80") +
  annotate("text", x = min(df$ancho, na.rm = TRUE), y = max(df$altura, na.rm = TRUE), 
           label = label_global, parse = TRUE, hjust = 0, size = 4.5, fontface = "bold") +
  scale_color_manual(name = "Estado", values = col_pap) +
  scale_shape_manual(name = "Estado", values = shp_pap) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none", axis.title = element_text(face = "bold"), plot.title = element_text(face = "bold")) +
  labs(title = "A. Regresión Morfométrica Global", subtitle = "Relación base Ancho vs Altura", x = "Ancho (w) [mm]", y = "Altura (h) [mm]")

# 4. GRÁFICA B: Regresión Estratificada
p_grupos <- ggplot(df, aes(x = ancho, y = altura, color = estado, shape = estado)) +
  geom_point(alpha = 0.7, size = 2.2) +
  geom_smooth(aes(fill = estado), method = "lm", alpha = 0.15, linewidth = 1.2, se = TRUE) +
  geom_text(data = df_r2, aes(x = x_pos, y = y_pos, label = label, color = estado), 
            parse = TRUE, hjust = 0, size = 4, show.legend = FALSE, fontface = "bold", inherit.aes = FALSE) +
  scale_color_manual(name = "Estado", values = col_pap) +
  scale_fill_manual(name = "Estado", values = col_pap) +
  scale_shape_manual(name = "Estado", values = shp_pap) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom", axis.title = element_text(face = "bold"), plot.title = element_text(face = "bold")) +
  labs(title = "B. Regresión Estratificada por Viabilidad", subtitle = "Descomposición de trayectorias alométricas", x = "Ancho (w) [mm]", y = "Altura (h) [mm]")

# 5. ENSAMBLE Y EXPORTACIÓN
figura_regresion <- (p_global | p_grupos) +
  plot_annotation(title = "Figura 4. Análisis Alométrico: Transversalidad y Supervivencia",
                  caption = "Nota: Las bandas de sombra representan el error estándar (SE). Textos muestran coeficientes de determinación (R²).") &
  theme(plot.title = element_text(face = "bold", size = 14))

# EXPORTACIÓN DESCOMENTADA Y CORREGIDA
ggsave("figura_04_alometria_regresion.jpg", plot = figura_regresion, path = "pics", width = 12, height = 6.5, dpi = 300)