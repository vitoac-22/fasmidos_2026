# ============================================================
# SCRIPT DE INICIALIZACIÓN DE PROYECTO: fasmidos_2026
# ============================================================

# Definición de la estructura de carpetas
directorios <- c(
  "data/raw",
  "data/processed",
  "R",
  "pics",
  "reports",
  "outputs"
)

# Función para crear directorios si no existen
crear_estructura <- function(carpetas) {
  for (carpeta in carpetas) {
    if (!dir.exists(carpeta)) {
      dir.create(carpeta, recursive = TRUE)
      message(paste("Directorio creado:", carpeta))
    } else {
      message(paste("El directorio ya existe:", carpeta))
    }
  }
}

# Ejecución de la creación
crear_estructura(directorios)

# Creación de archivos base opcionales
if (!file.exists("README.md")) {
  file.create("README.md")
  writeLines("# Proyecto Fásmidos 2026\nAnálisis morfométrico de Dyme ramulus.", "README.md")
}

message("\nEstructura de datos lista. Proyecto inicializado con éxito.")