# ============================================================
# PROYECTO: Fásmidos 2026
# SCRIPT: 01_limpieza_biometria.R
# DESCRIPCIÓN: Carga de datos crudos, corrección de estados y 
#              exportación a datos procesados.
# ============================================================

# 1. CARGA DE DATOS (Desde data/raw)
# ------------------------------------------------------------
# Asegúrate de que el archivo esté físicamente en fasmidos_2026/data/raw/
raw_path <- "data/raw/p_EGGS_EJES.xlsx"

if (!file.exists(raw_path)) {
  stop("ERROR CRÍTICO: El archivo original no se encuentra en data/raw/. Verifica la ruta.")
}

df_raw <- readxl::read_excel(raw_path, sheet = "DATA_MOD_01")

# 2. LIMPIEZA LÓGICA Y RECLASIFICACIÓN
# ------------------------------------------------------------
df_processed <- df_raw |> 
  dplyr::mutate(
    estado = dplyr::case_when(
      estado == "Adulto" & eclosion == "SI" & vivos == "NO" ~ "Eclosionó",
      estado == "Eclosion" & eclosion == "SI" & vivos == "SI" ~ "Adulto",
      estado == "Eclosion" ~ "Eclosionó",
      estado == "No viable" ~ "No eclosionó",
      TRUE ~ estado
    ),
    estado = factor(estado, levels = c("No eclosionó", "Eclosionó", "Adulto"))
  )

# 3. EXPORTACIÓN (Hacia data/processed)
# ------------------------------------------------------------
# Guardamos en formato .rds para preservar los factores y la estructura de R
# Guardamos también en .csv por si necesitas consultarlo externamente

saveRDS(df_processed, "data/processed/fasmidos_clean.rds")
readr::write_csv(df_processed, "data/processed/fasmidos_clean.csv")

message("PROCESAMIENTO COMPLETADO.")
message("Archivo RDS guardado en: data/processed/fasmidos_clean.rds")
message("Archivo CSV guardado en: data/processed/fasmidos_clean.csv")