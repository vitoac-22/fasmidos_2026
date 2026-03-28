# ============================================================
# PROYECTO: Fásmidos 2026
# SCRIPT: 03_visualizacion_cientifica.R
# DESCRIPCIÓN: Generación de figuras multipanel, diagramas de 
#              restricción y visualización bayesiana para el paper.
# ============================================================

# 1. CARGA DE DEPENDENCIAS Y DATOS
# ------------------------------------------------------------
# CORRECCIÓN: Se añade 'tidyr' para la manipulación de datos largos
paquetes <- c("dplyr", "tidyr", "ggplot2", "patchwork", "ggridges", "ggtern", "scales")
invisible(lapply(paquetes, library, character.only = TRUE))

# Carga de datos limpios
df <- readRDS("data/processed/fasmidos_clean.rds") |>
  mutate(
    eclosion_bin = factor(ifelse(estado == "No eclosionó", "Fallo", "Éxito"), 
                          levels = c("Fallo", "Éxito"))
  )

# Paletas Muted certificadas para paper Q1
col_bin <- c("Fallo" = "#e07a5f", "Éxito" = "#81b29a") # Terracota y Salvia
col_tri <- c("No eclosionó" = "#e07a5f", "Eclosionó" = "#457b9d", "Adulto" = "#2E7D32")
shp_tri <- c("No eclosionó" = 4, "Eclosionó" = 21, "Adulto" = 24)

# ============================================================
# FIGURA 1: VIABILIDAD BINARIA (ÉXITO VS FALLO) - McElreath Style
# ============================================================
# No queremos ver medias, queremos ver la DENSIDAD de la mortalidad.
df_long_bin <- df |>
  select(eclosion_bin, altura, ancho) |>
  pivot_longer(cols = c(altura, ancho), names_to = "dimension", values_to = "medida")


figura_1 <- ggplot(df_long_bin, aes(x = medida, y = eclosion_bin, fill = eclosion_bin)) +
  geom_density_ridges(alpha = 0.8, scale = 1.1) +
  facet_wrap(~dimension, scales = "free_x", labeller = as_labeller(c(altura = "Altura (h)", ancho = "Ancho (w)"))) +
  scale_fill_manual(values = col_bin) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey95"),
    strip.text = element_text(face = "bold")
  ) +
  labs(title = "Análisis de Densidad de Viabilidad Embrionaria",
       subtitle = "Obsérvese el desplazamiento de la sección transversal en los individuos exitosos",
       x = "Medida [mm]", y = "Destino del Individuo")

ggsave("pics/figura_1_binaria_ridges.tiff", figura_1, width = 8, height = 4.5, dpi = 300, compression = "lzw")

# ============================================================
# FIGURA 2: ONTOGENIA COMPLETA (LOS 3 ESTADOS) - Tidyplots/Patchwork
# ============================================================
func_boxplot <- function(var_str, lab_y, leyenda = FALSE) {
  p <- ggplot(df, aes(x = estado, y = .data[[var_str]], fill = estado, shape = estado)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.6) +
    geom_jitter(alpha = 0.4, width = 0.15) +
    scale_fill_manual(values = col_tri) +
    scale_shape_manual(values = shp_tri) +
    theme_minimal() +
    theme(axis.title.y = element_text(face = "bold")) +
    labs(y = lab_y, x = "")
  
  if (!leyenda) p <- p + theme(legend.position = "none")
  return(p)
}

p_alt <- func_boxplot("altura", "Altura (h) [mm]", FALSE)
p_anc <- func_boxplot("ancho", "Ancho (w) [mm]", FALSE)
p_lon <- func_boxplot("longitud", "Longitud (l) [mm]", TRUE)


figura_2 <- (p_alt | p_anc | p_lon) +
  plot_annotation(
    title = "Efectos Morfométricos a través de la Ontogenia",
    subtitle = "Doble codificación (forma/color) para accesibilidad universal",
    tag_levels = "A"
  ) & theme(legend.position = "bottom")

ggsave("pics/figura_2_tri_paneles.tiff", figura_2, width = 11, height = 5, dpi = 300, compression = "lzw")

# ============================================================
# FIGURA 3: COMPARATIVA ADULTO VS ECLOSIONÓ (SUBSET EXITOSOS)
# ============================================================
df_exito <- df |> filter(estado != "No eclosionó")


figura_3 <- ggplot(df_exito, aes(x = ancho, y = estado, fill = estado)) +
  geom_density_ridges(alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white", color = "black") +
  geom_jitter(height = 0.1, alpha = 0.3) +
  scale_fill_manual(values = col_tri) +
  theme_ridges() +
  theme(legend.position = "none") +
  labs(title = "Distribución de Ancho en Población Exitosa",
       subtitle = "Comparativa de ninfas (Eclosionó) y éxito total (Adulto)",
       x = "Ancho (w) [mm]", y = "")

ggsave("pics/figura_3_exitos_raincloud.png", figura_3, width = 7, height = 4.5, dpi = 300)

# ============================================================
# FIGURA 4 (MCELREATH): EL TERNARIO DE RESTRICCIÓN MECÁNICA
# ============================================================
df_tern <- df |>
  mutate(total = altura + ancho + longitud,
         p_alt = altura/total,
         p_anc = ancho/total,
         p_lon = longitud/total)


figura_4 <- ggtern(data = df_tern, aes(x = p_lon, y = p_alt, z = p_anc, color = estado, shape = estado)) +
  geom_point(alpha = 0.5, size = 2) +
  scale_color_manual(values = col_tri) +
  scale_shape_manual(values = shp_tri) +
  theme_classic() +
  theme(
    tern.axis.title.T = element_text(face = "bold"),
    tern.axis.title.L = element_text(face = "bold"),
    tern.axis.title.R = element_text(face = "bold")
  ) +
  labs(title = "Espacio Morfométrico de Viabilidad",
       subtitle = "Diagrama ternario de proporciones. Los adultos se concentran en el equilibrio Transversal",
       T = "Longitud", L = "Altura", R = "Ancho")

ggsave("pics/figura_4_ternario_causal.png", figura_4, width = 8, height = 7, dpi = 300)

print("Todas las figuras han sido generadas y guardadas en la carpeta 'pics/'.")