# Archivo: R/ternario_huevos.R

# Si no las tienes, instala las dependencias estables:
# install.packages(c("readxl", "dplyr", "plotly"))

library(readxl)
library(dplyr)
library(plotly)

# 1. Extracción pura. Dejamos el Excel intacto en su rincón.
# Asumo que la pestaña que tiene los datos limpios se llama "DATA_MOD_01" o ajusta el nombre.
df <- read_excel("data/p_EGGS_EJES.xlsx", sheet = "DATA_MOD_01")
head(df)

# 2. Generación del gráfico ternario. 
# Plotly se encarga de la normalización matemática (a+b+c = 1 o 100%) por detrás. 
# Cero cálculos manuales, menos margen de error.



fig <- plot_ly(
  data = df,
  a = ~altura,
  b = ~longitud,
  c = ~ancho,
  color = ~parejas,       # Separación visual por parejas
  symbol = ~estado,       # Diferenciación por viabilidad
  type = "scatterternary",
  mode = "markers",
  marker = list(size = 8, line = list(width = 1, color = 'rgba(0,0,0,0.5)')),
  # El hover es clave para el negocio: muestra la data cruda sin ensuciar la visualización
  hoverinfo = "text",
  text = ~paste("<b>Estado:</b>", estado, 
                "<br><b>Pareja:</b>", parejas,
                "<br>Altura:", altura, 
                "<br>Longitud:", longitud, 
                "<br>Ancho:", ancho)
)

# 3. Maquillaje de la estructura
fig <- fig %>% layout(
  title = "Proporciones Morfológicas de Huevos (h, l, w)",
  ternary = list(
    sum = 100,
    aaxis = list(title = "Altura"),
    baxis = list(title = "Longitud"),
    caxis = list(title = "Ancho")
  ),
  paper_bgcolor = "white",
  plot_bgcolor = "white"
)

# Ejecuta y observa el navegador
fig
