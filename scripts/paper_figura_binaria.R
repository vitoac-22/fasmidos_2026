# Archivo: R/paper_figura_binaria.R

# Dependencias estables. Usa install.packages("tidyr") si no la tienes.
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

# 1. Carga de datos
df <- read_excel("data/p_EGGS_EJES.xlsx", sheet = "DATA_MOD_01")

# 2. Ingeniería de Características: La variable objetivo real (Binaria)
df_binario <- df %>%
  mutate(exito_eclosion = ifelse(estado == "No viable", "Fallo", "Éxito")) %>%
  mutate(exito_eclosion = factor(exito_eclosion, levels = c("Fallo", "Éxito")))

# --- 3. PRUEBAS DE HIPÓTESIS (t de Welch para varianzas desiguales) ---
cat("\n=== PRUEBAS T DE WELCH: PREDICTORES MORFOLÓGICOS DE ECLOSIÓN ===\n")

cat("\n--- 1. ANCHO (w) ---\n")
print(t.test(ancho ~ exito_eclosion, data = df_binario))

cat("\n--- 2. ALTURA (h) ---\n")
print(t.test(altura ~ exito_eclosion, data = df_binario))

cat("\n--- 3. LONGITUD (l) ---\n")
print(t.test(longitud ~ exito_eclosion, data = df_binario))

# --- 4. PREPARACIÓN DE DATOS PARA EL VISUAL MULTIPANEL ---
# Pivotamos los datos a formato largo (tidy data) para crear un facet_wrap impecable
df_long <- df_binario %>%
  select(exito_eclosion, ancho, altura, longitud) %>%
  pivot_longer(cols = c(ancho, altura, longitud), 
               names_to = "dimension", 
               values_to = "medida") %>%
  # Ordenamos para el paper (De mayor significancia a menor significancia)
  mutate(dimension = factor(dimension, 
                            levels = c("ancho", "altura", "longitud"),
                            labels = c("Ancho (w) ***", "Altura (h) ***", "Longitud (l) - NS")))

# --- 5. GRÁFICA ACADÉMICA (ESTILO NATURE/SCIENCE) ---
figura_final <- ggplot(df_long, aes(x = exito_eclosion, y = medida, fill = exito_eclosion)) +
  # Combinación letal para papers: Violín (distribución) + Boxplot (cuartiles)
  geom_violin(trim = FALSE, alpha = 0.4, color = NA) +
  geom_boxplot(width = 0.2, fill = "white", color = "black", 
               outlier.shape = 21, outlier.fill = "black", outlier.size = 1.5, linewidth = 0.5) +
  
  # Paneles separados por cada dimensión, escalas libres porque miden cosas distintas
  facet_wrap(~dimension, scales = "free_y") +
  
  # Escala de grises contrastante (Éxito = Gris Oscuro, Fallo = Gris Claro)
  scale_fill_manual(values = c("Fallo" = "grey80", "Éxito" = "grey30")) +
  
  # Estética estricta para revistas indexadas
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none", # Fuera leyendas, los ejes ya lo explican
    strip.background = element_rect(fill = "grey95", color = "black", linewidth = 1),
    strip.text = element_text(face = "bold", size = 11),
    axis.text.x = element_text(color = "black", face = "bold"),
    axis.text.y = element_text(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  labs(
    x = "Resultado de Eclosión",
    y = "Dimensión Morfológica [mm]"
  )

# Mostrar en consola
print(figura_final)

# Exportar en Alta Resolución
ggsave("figura_paper_binaria_paneles.tiff", plot = figura_final, 
       width = 8, height = 4.5, dpi = 300, compression = "lzw")