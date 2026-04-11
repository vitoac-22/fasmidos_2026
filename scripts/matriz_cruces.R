# Archivo: R/matriz_base.R
library(readxl)

# 1. Lectura de datos
df <- read_excel("data/p_EGGS_EJES.xlsx", sheet = "DATA_MOD_01")

# 2. Aislamiento de variables
metricas <- df[, c("altura", "longitud", "ancho")]
estados <- as.factor(df$estado)

# 3. Mapeo de colores (Transparencia simulada con hex codes + canal alfa)
# Asumiendo 3 estados: Adulto, Eclosion, No viable. 
mis_colores <- c("#00cc9688", "#636efa88", "#ef553b88") 
colores_puntos <- mis_colores[as.numeric(estados)]

# --- CONSTRUCCIÓN DE LA HERRAMIENTA ---

# Función matemática para el panel superior: Calcula y grafica la Correlación
panel.cor <- function(x, y, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use = "complete.obs")
  txt <- sprintf("%.2f", r)
  # Si la correlación es altísima (>0.7), te la pinto de rojo para que la notes
  color_texto <- ifelse(abs(r) > 0.7, "red", "black")
  # El número crece en tamaño según la fuerza de la correlación
  text(0.5, 0.5, txt, cex = 1 + abs(r)*1.5, font = 2, col = color_texto)
}

# Función para el panel inferior: Dispersión pura
panel.scatter <- function(x, y, ...) {
  points(x, y, col = colores_puntos, pch = 16, cex = 1.2)
}

# Función para la diagonal: Distribución de frecuencias (Histogramas)
panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "grey90", border = "white")
}

# --- RENDERIZADO ---

# Limpiar los márgenes del entorno gráfico de R por si estaba sucio
par(mar = c(4, 4, 4, 8), xpd = TRUE)

# Ejecutamos la matriz
pairs(metricas,
      lower.panel = panel.scatter,
      upper.panel = panel.cor,
      diag.panel = panel.hist,
      main = "Matriz Morfológica (Lógica R Base - Cero Dependencias)",
      labels = c("Altura", "Longitud", "Ancho"),
      gap = 0.5) # Espacio entre paneles

# Añadimos la leyenda a la fuerza fuera del área de trazado
legend("topright", inset = c(-0.25, 0), 
       legend = levels(estados), 
       fill = mis_colores, 
       title = "Estado Vital", 
       bty = "n", cex = 0.9)