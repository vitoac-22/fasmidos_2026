# ============================================================
# 0. CARGA DE DEPENDENCIAS
# ============================================================
paquetes <- c("dplyr","tidyr","ggplot2","car","readxl","patchwork")
faltantes <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if(length(faltantes)) install.packages(faltantes)
invisible(lapply(paquetes, library, character.only = TRUE))

# ============================================================
# 1. CARGA Y LIMPIEZA LÓGICA
# ============================================================
df_raw <- read_excel("data/raw/p_EGGS_EJES.xlsx", sheet = "DATA_MOD_01")
df <- df_raw %>%
  mutate(
    estado = case_when(
      estado == "Adulto" & eclosion == "SI" & vivos == "NO" ~ "Eclosionó",
      estado == "Eclosion" & eclosion == "SI" & vivos == "SI" ~ "Adulto",
      estado == "Eclosion" ~ "Eclosionó",
      estado == "No viable" ~ "No eclosionó",
      TRUE ~ estado
    ),
    estado = factor(estado, levels = c("No eclosionó","Eclosionó","Adulto"))
  )

# ============================================================
# 2. CONFIGURACIÓN ESTÉTICA (LA SOLUCIÓN ARQUITECTÓNICA)
# ============================================================
# Separamos las paletas para no tener que hackear la leyenda después
paleta_relleno <- c("No eclosionó" = "#e07a5f", "Eclosionó" = "#457b9d", "Adulto" = "#81b29a")
paleta_borde   <- c("No eclosionó" = "#e07a5f", "Eclosionó" = "black",   "Adulto" = "black")
formas_paper   <- c("No eclosionó" = 4,         "Eclosionó" = 21,        "Adulto" = 24)

# ============================================================
# 3. FUNCIÓN GENERADORA (Base sólida)
# ============================================================
crear_cruce <- function(df, x_var, y_var, x_lab, y_lab, mostrar_leyenda = FALSE) {
  p <- ggplot(df, aes(x=.data[[x_var]], y=.data[[y_var]], 
                      color=estado, shape=estado, fill=estado)) +
    
    # Capa 1: Fallos (Usará la paleta de bordes automáticamente)
    geom_point(data=filter(df, estado=="No eclosionó"), alpha=0.35, size=1.8) +
    
    # Capa 2: Éxitos (Usará la paleta de relleno y la de bordes negros automáticamente)
    geom_point(data=filter(df, estado!="No eclosionó"), alpha=0.9, size=2.6, stroke=0.8) +
    
    # Mapeo idéntico de nombres para forzar la fusión perfecta de la leyenda
    scale_color_manual(name="Estado", values=paleta_borde) +
    scale_fill_manual(name="Estado", values=paleta_relleno) +
    scale_shape_manual(name="Estado", values=formas_paper) +
    
    theme_bw(base_size=11) +
    theme(
      panel.grid.minor=element_blank(),
      axis.title=element_text(face="bold"),
      panel.border=element_rect(linewidth=1)
    ) +
    labs(x=x_lab, y=y_lab)
  
  # Control paramétrico de la leyenda
  if (!mostrar_leyenda) {
    p <- p + theme(legend.position="none")
  }
  return(p)
}

# ============================================================
# 4. CONSTRUCCIÓN DE PANELES
# ============================================================
# Paneles 1 y 2 sin leyenda
p1 <- crear_cruce(df, "longitud", "altura", "Longitud (l) [mm]", "Altura (h) [mm]", FALSE)
p2 <- crear_cruce(df, "ancho", "altura", "Ancho (w) [mm]", "Altura (h) [mm]", FALSE)

# Panel 3 pidiendo leyenda, y luego ajustando su posición interna
p3 <- crear_cruce(df, "longitud", "ancho", "Longitud (l) [mm]", "Ancho (w) [mm]", TRUE) +
  theme(
    legend.position=c(0.78, 0.18),
    legend.title=element_text(face="bold", size=9),
    legend.text=element_text(size=8),
    legend.background=element_rect(fill=alpha("white",0.7), color="grey80"),
    legend.key.size=unit(0.4, "cm")
  )

# ============================================================
# 5. COMPOSICIÓN Y EXPORTACIÓN
# ============================================================
figura_completa <- (p1 | p2 | p3) +
  plot_annotation(
    title="Figura 4. Análisis de Dispersión Morfométrica de Dyme ramulus",
    subtitle="Cruces bivariados de los ejes h, l, w (N=338). Doble codificación: Color y Forma.",
    caption="Nota: Las 'X' representan fallos de eclosión. El color verde salvia denota éxito total (Adulto).",
    tag_levels="A",
    theme=theme(
      plot.title=element_text(face="bold", size=15),
      plot.subtitle=element_text(size=11, color="grey30")
    )
  )

print(figura_completa)
# 
# ggsave("Figura4_Dyme_ramulus_Final.tiff",
#        path = "pics",
#        figura_completa,
#        width=11,
#        height=4.5,
#        dpi=300,
#        compression="lzw")
