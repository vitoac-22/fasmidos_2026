# Archivo: R/scatter3d_huevos.R

# Librerías estables y directas
library(readxl)
library(dplyr)
library(plotly)

# Extracción de datos con la ruta exacta que especificaste
df <- read_excel("data/p_EGGS_EJES.xlsx", sheet = "DATA_MOD_01")

# Recreación de la relación h-l-w en un entorno 3D real
# Usamos opacidad y tamaño reducido para destruir la "colada de datos"
fig <- plot_ly(
  data = df, 
  x = ~longitud, 
  y = ~ancho, 
  z = ~altura, 
  color = ~estado, 
  # Paleta de colores de alto contraste para separar los estados vitales
  colors = c("#ef553b", "#00cc96", "#636efa"), 
  symbol = ~parejas,
  symbols = c('circle', 'square', 'diamond'),
  type = "scatter3d", 
  mode = "markers",
  marker = list(
    size = 4,             # Puntos más pequeños para reducir colisiones
    opacity = 0.65,       # Clave: transparencia para ver la densidad real
    line = list(color = 'rgba(255, 255, 255, 0.4)', width = 0.5)
  ),
  hoverinfo = 'text',
  text = ~paste("<b>Estado:</b>", estado, 
                "<br><b>Pareja:</b>", parejas,
                "<br>L:", longitud, 
                "| W:", ancho, 
                "| H:", altura)
)

# Estructuración visual para que no parezca un borrador
fig <- fig %>% layout(
  title = "Distribución Morfológica 3D (Longitud vs Ancho vs Altura)",
  scene = list(
    xaxis = list(title = "Longitud (l)"),
    yaxis = list(title = "Ancho (w)"),
    zaxis = list(title = "Altura (h)")
  ),
  paper_bgcolor = "white"
)

# Renderizar
fig