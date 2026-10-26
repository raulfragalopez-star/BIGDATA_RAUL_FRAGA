# ==============================================================================
# SCRIPT 1: UNIFICACIÓN DE DATASETS (BIG DATA INTEGRATION)
# ==============================================================================

# Cargar la librería estrella
library(tidyverse)

# 1. CONFIGURACIÓN DEL ENTORNO
# Definimos la ruta raíz donde están las carpetas de las regiones
RUTA_BASE <- "C:/Users/victo/Desktop/Personal/Cursos/Big Data/Sudamerica" # Usa barras normales / en R

# Lista para almacenar las columnas críticas que queremos conservar
COLUMNAS_CLAVE <- c(
  'id', 'listing_url', 'name', 'host_id', 'host_name', 'host_is_superhost',
  'neighbourhood_cleansed', 'latitude', 'longitude', 'room_type', 
  'accommodates', 'bathrooms_text', 'bedrooms', 'beds', 'price', 
  'number_of_reviews', 'review_scores_rating'
)

print("🚀 Iniciando el proceso de unificación de datos...")

# Obtener los nombres de las subcarpetas dentro del directorio principal
carpetas_region <- list.dirs(RUTA_BASE, full_names = FALSE, recursive = FALSE)

lista_dataframes <- list()

# 2. BUCLE ETL: RECORRER CARPETAS POR REGIÓN
for (carpeta in carpetas_region) {
  ruta_completa <- file.path(RUTA_BASE, carpeta)
  archivo_listings <- file.path(ruta_completa, "listings.csv")
  
  # Verificamos si el archivo listings.csv existe en esa carpeta
  if (file.exists(archivo_listings)) {
    print(paste("📦 Procesando región:", toupper(carpeta), "..."))
    
    tryCatch({
      # LECTURA OPTIMIZADA: Cargamos todo y luego seleccionamos columnas clave
      df_region <- read_csv(archivo_listings, col_types = cols(.default = "c")) %>% 
        select(any_of(COLUMNAS_CLAVE))
      
      # TRANSFORMACIÓN: Añadimos la columna de origen para mantener la trazabilidad
      # Convierte la primera letra en mayúscula (ej: "bogota" -> "Bogota")
      df_region <- df_region %>% 
        mutate(region_origen = str_to_title(carpeta))
      
      # Guardamos el DataFrame temporal en nuestra lista
      lista_dataframes[[carpeta]] <- df_region
      print(paste("   ✅ Leídas", nrow(df_region), "filas con éxito."))
      
    }, error = function(e) {
      print(paste("   ❌ Error al procesar", carpeta, ":", e$message))
    })
  }
}

# 3. CONSOLIDACIÓN
if (length(lista_dataframes) > 0) {
  print("\n🔄 Uniendo todos los datasets en un archivo único maestro...")
  # Concatenamos verticalmente todos los DataFrames de la lista
  df_master <- bind_rows(lista_dataframes)
  
  # 4. LIMPIEZA INICIAL DE DATOS
  print("🧹 Aplicando limpieza inicial...")
  # Eliminamos duplicados si existieran por ID de alojamiento
  df_master <- df_master %>% distinct(id, .keep_all = TRUE)
  
  # Tratamiento del precio: Inside Airbnb lo entrega como texto (ej: "$50.00"). Lo convertimos a número.
  df_master <- df_master %>% 
    mutate(
      price = str_replace_all(price, fixed("$"), ""),
      price = str_replace_all(price, fixed(","), ""),
      price = as.numeric(price)
    )
  
  # 5. ALMACENAMIENTO DE LA CARGA (LOAD)
  archivo_salida <- "airbnb_sudamerica_master.csv"
  write_csv(df_master, archivo_salida)
  
  print("\n🎉 ¡Proceso completado con éxito!")
  print(paste("📊 Total de alojamientos consolidados:", nrow(df_master), "filas."))
  print(paste("💾 Archivo guardado como:", archivo_salida))
} else {
  print("⚠️ No se encontraron archivos 'listings.csv' para unificar.")
}

