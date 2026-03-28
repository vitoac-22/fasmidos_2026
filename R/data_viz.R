# ============================================================
# 1. PREPARACIÓN Y PALETA
# ============================================================
library(dplyr)
library(readxl)
library(tidyplots)   # v0.4.0.9000 (GitHub)
library(ggplot2)
library(patchwork)

# Paleta con alto contraste perceptual (funciona en B&W también)
col_pap <- c(
  "No eclosionó" = "#BDBDBD",   # gris claro  → fondo/ruido
  "Eclosionó"    = "#1565C0",   # azul profundo → evento clave
  "Adulto"       = "#C62828"    # rojo          → categoría de mayor interés
)

# Formas con relleno diferenciado
shp_pap <- c(
  "No eclosionó" = 1,    # círculo vacío   ○
  "Eclosionó"    = 16,   # círculo sólido  ●
  "Adulto"       = 17    # triángulo sólido ▲
)

# ============================================================
# 2. FUNCIÓN DE PANELES
# ============================================================
crear_panel <- function(data, x_var, y_var, x_lab, y_lab, mostrar_leyenda = FALSE) {
  
  p <- data |>
    tidyplot(x = {{x_var}}, y = {{y_var}}, color = estado, shape = estado) |>
    add_data_points(alpha = 0.65, size = 2.2) |>
    adjust_colors(col_pap) |>
    adjust_x_axis_title(x_lab) |>
    adjust_y_axis_title(y_lab) |>
    adjust_legend_title("Estado")
  
  if (!mostrar_leyenda) {
    p <- p |> remove_legend()
  }
  
  return(p)
}

# ============================================================
# 3. GENERACIÓN DE PANELES
# ============================================================
# Panel A: Longitud vs Altura
p1 <- df |> crear_panel(
  longitud, altura,
  "Longitud (l) [mm]", "Altura (h) [mm]",
  mostrar_leyenda = FALSE
)

# Panel B: Ancho vs Altura
p2 <- df |> crear_panel(
  ancho, altura,
  "Ancho (w) [mm]", "Altura (h) [mm]",
  mostrar_leyenda = FALSE
)

# Panel C: Longitud vs Ancho (lleva la leyenda)
p3 <- df |> crear_panel(
  longitud, ancho,
  "Longitud (l) [mm]", "Ancho (w) [mm]",
  mostrar_leyenda = TRUE
)

# ============================================================
# 4. COMPOSICIÓN FINAL CON PATCHWORK
# ============================================================
figura_final <- (p1 | p2 | p3) +
  plot_annotation(
    title    = "Figura 4. Biometría y Viabilidad en Dyme ramulus",
    subtitle = "Cruces de variables para la determinación de umbrales físicos (N=338)",
    tag_levels = "A"
  ) &
  theme_bw(base_size = 12) &
  theme(
    legend.position  = "bottom",
    legend.key.size  = unit(0.6, "cm"),
    legend.text      = element_text(size = 11),
    legend.title     = element_text(face = "bold", size = 11),
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 11, color = "grey30"),
    axis.title       = element_text(face = "bold"),
    axis.text        = element_text(size = 10),
    panel.grid.minor = element_blank(),   # menos ruido visual
    strip.background = element_blank()
  ) &
  scale_shape_manual(
    name   = "Estado",
    values = shp_pap
  ) &
  guides(
    color = guide_legend(
      "Estado",
      override.aes = list(size = 4, alpha = 1)   # íconos grandes y opacos en leyenda
    ),
    shape = guide_legend(
      "Estado",
      override.aes = list(size = 4, alpha = 1)
    )
  )

# ============================================================
# 5. RENDERIZADO
# ============================================================
figura_final |> view_plot()

# Opcional: exportar a archivo
ggsave(
  "figura4_biometria_viabilidad.png",
  plot   = figura_final +
    theme(
      legend.text  = element_text(size = 12),
      legend.title = element_text(size = 13),
      legend.key.size = unit(1.2, "lines"),
      legend.position = "bottom"
    ) +
    guides(color = guide_legend(ncol = 2),
           shape = guide_legend(ncol = 2)),
  width  = 6.5,   # ~16.5 cm
  height = 6,     # ~15 cm
  dpi    = 300,
  bg     = "white",
  path   = "pics"
)

