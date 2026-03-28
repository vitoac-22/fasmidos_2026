# Archivo: R/cruce_1_l_vs_w.R

library(readxl)
library(plotly)

# 1. Extracción de los datos
df <- read_excel("data/p_EGGS_EJES.xlsx", sheet = "DATA_MOD_01")

# 2. Paleta Okabe-Ito (Ingeniería de accesibilidad pura)
# Asignamos colores fijos por estado para evitar que Plotly los rote al azar
colores_accesibles <- c(
  "No viable" = "#D55E00",  # Bermellón (Contraste alto para fallos)
  "Adulto"    = "#009E73",  # Verde azulado (Éxito)
  "Eclosion"  = "#56B4E9"   # Azul cielo (Transición)
)

# 3. Formas (Doble codificación para el estado)
formas_accesibles <- c(
  "No viable" = "x",        # Una 'X' grita "error" o "descarte" en cualquier idioma
  "Adulto"    = "diamond",  # Diamante para el valor más alto del negocio
  "Eclosion"  = "circle"    # Círculo para estado intermedio
)

# 4. Construcción del motor gráfico
fig <- plot_ly(
  data = df,
  x = ~longitud,
  y = ~ancho,
  color = ~estado,
  colors = colores_accesibles,
  symbol = ~estado,         # Aquí está la magia: la misma variable dicta la forma
  symbols = formas_accesibles,
  type = "scatter",
  mode = "markers",
  marker = list(
    size = 10,              # Ligeramente más grandes para distinguir las formas
    opacity = 0.8, 
    line = list(width = 1, color = 'rgba(0,0,0,0.5)') # Borde para evitar que se fusionen
  ),
  hoverinfo = 'text',
  text = ~paste("<b>Estado:</b>", estado, 
                "<br><b>Pareja:</b>", parejas,
                "<br>Longitud:", longitud, 
                "<br>Ancho:", ancho)
)

# 5. Estética corporativa/académica
fig <- fig %>% layout(
  title = "Frontera Morfológica: Longitud vs Ancho (Accesibilidad Total)",
  xaxis = list(title = "Longitud (l)", zeroline = FALSE),
  yaxis = list(title = "Ancho (w)", zeroline = FALSE),
  plot_bgcolor = "#f8f9fa",
  paper_bgcolor = "white",
  legend = list(title = list(text = '<b>Estado Vital</b>'))
)

# Renderizar
fig