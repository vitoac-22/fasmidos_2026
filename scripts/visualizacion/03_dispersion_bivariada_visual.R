# ============================================================
# SCRIPT: 03_dispersion_bivariada_visual.R
# DESCRIPCIÓN: Análisis de dispersión bivariada (h, l, w) con 
#              corrección de superposición (jitter) para N=338.
# ============================================================
library(dplyr)
library(ggplot2)
library(patchwork)

df <- readRDS("data/processed/fasmidos_clean.rds")

# Paleta Estandarizada Q1 (Sincronizada con los violines)
col_pap <- c("No eclosionó" = "#BDBDBD", "Eclosionó" = "#1565C0", "Adulto" = "#C62828")
shp_pap <- c("No eclosionó" = 1, "Eclosionó" = 16, "Adulto" = 17)

crear_cruce <- function(df, x_var, y_var, x_lab, y_lab, mostrar_leyenda = FALSE) {
  p <- ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]], 
                      color = estado, shape = estado)) + # Eliminamos 'fill'
    
    # Capa 1: Fallos (Estáticos)
    geom_point(data = filter(df, estado == "No eclosionó"), alpha = 0.6, size = 1.8) +
    
    # Capa 2: Éxitos y Adultos (Jitter corregido con 'position_jitter')
    geom_point(data = filter(df, estado != "No eclosionó"), 
               alpha = 0.85, size = 2.6, 
               position = position_jitter(width = 0.015, height = 0.015, seed = 42)) +
    
    scale_color_manual(name = "Estado", values = col_pap) +
    scale_shape_manual(name = "Estado", values = shp_pap) +
    
    theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank(), axis.title = element_text(face = "bold"),
          panel.border = element_rect(linewidth = 1)) +
    labs(x = x_lab, y = y_lab)
  
  if (!mostrar_leyenda) p <- p + theme(legend.position = "none")
  return(p)
}

p1 <- crear_cruce(df, "longitud", "altura", "Longitud (l) [mm]", "Altura (h) [mm]", FALSE)
p2 <- crear_cruce(df, "ancho", "altura", "Ancho (w) [mm]", "Altura (h) [mm]", FALSE)
p3 <- crear_cruce(df, "longitud", "ancho", "Longitud (l) [mm]", "Ancho (w) [mm]", TRUE) +
  theme(legend.position = c(0.78, 0.18), legend.title = element_text(face = "bold", size = 9),
        legend.text = element_text(size = 8), legend.background = element_rect(fill = alpha("white", 0.7), color = "grey80"),
        legend.key.size = unit(0.4, "cm"))

figura_completa <- (p1 | p2 | p3) +
  plot_annotation(
    title = "Figura 1. Análisis de Dispersión Morfométrica",
    subtitle = "Cruces bivariados de los ejes h, l, w (N=338).",
    tag_levels = "A", theme = theme(plot.title = element_text(face = "bold", size = 15))
  )

ggsave("figura_01_dispersion_morfometrica.jpg", plot = figura_completa, path = "pics", width = 11, height = 4.5, dpi = 300)