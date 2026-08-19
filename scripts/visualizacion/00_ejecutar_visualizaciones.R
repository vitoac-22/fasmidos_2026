# ============================================================
# SCRIPT MAESTRO: 00_ejecutar_visualizaciones.R
# DESCRIPCIÓN: Compilación automatizada de todo el pipeline 
#              gráfico del proyecto Fásmidos 2026.
# ============================================================

message("Iniciando pipeline visual...")

ruta_scripts <- "scripts/visualizacion/"

message("1/4 Generando dispersión bivariada (Figura 1)...")
source(paste0(ruta_scripts, "03_dispersion_bivariada_visual.R"))

message("2/4 Generando distribuciones y violines (Figura 2)...")
source(paste0(ruta_scripts, "04_viabilidad_violines.R"))

message("3/4 Generando trayectoria temporal ninfal (Figura 3)...")
source(paste0(ruta_scripts, "05_duracion_ninfal_visual.R"))

message("4/4 Generando modelos alométricos (Figura 4)...")
# Ajuste: Apunta al nombre correcto del archivo de alometría
source(paste0(ruta_scripts, "06_alometria_visual.R"))

message("Pipeline completado con éxito. Gráficas guardadas en el directorio 'pics/'.")