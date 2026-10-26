# ==============================================================================
# SCRIPT 2: PREPROCESAMIENTO AVANZADO — AIRBNB BENELUX
# ==============================================================================

# Ejecutar SOLO si faltan paquetes:
# install.packages(c("readxl", "tidyverse"))

library(readxl)
library(tidyverse)

# ==============================================================================
# 1. CARGAR EL EXCEL
# ==============================================================================

print("Selecciona el archivo Excel de Airbnb.")

ruta <- file.choose()
df <- read_excel(ruta)

stopifnot(is.data.frame(df))

filas_iniciales <- nrow(df)

print(paste("Archivo cargado. Filas:", filas_iniciales))

# Comprobar las columnas necesarias
columnas_necesarias <- c(
  "region_origen",
  "bedrooms",
  "beds",
  "review_scores_rating",
  "host_is_superhost",
  "price"
)

columnas_faltantes <- setdiff(columnas_necesarias, names(df))

if (length(columnas_faltantes) > 0) {
  stop(paste(
    "Faltan estas columnas:",
    paste(columnas_faltantes, collapse = ", ")
  ))
}

# Eliminar columnas sin encabezado SOLO si están completamente vacías
columnas_sin_nombre <- names(df)[
  grepl("^\\.\\.\\.[0-9]+$", names(df))
]

for (columna in columnas_sin_nombre) {
  valores <- trimws(as.character(df[[columna]]))
  
  if (all(is.na(valores) | valores == "")) {
    df[[columna]] <- NULL
  }
}

# ==============================================================================
# 2. CONVERTIR TEXTO A NÚMEROS
# ==============================================================================

# El archivo mostrado utiliza punto decimal: "1.0", "100.0".
# Si hay formatos diferentes, detenemos el proceso para revisarlos.

convertir_numero <- function(x, nombre_columna) {
  
  texto <- trimws(as.character(x))
  
  es_nulo <- is.na(texto) |
    toupper(texto) %in% c("", "NA", "N/A", "NULL", "NAN")
  
  texto[es_nulo] <- NA_character_
  
  numero <- suppressWarnings(as.numeric(texto))
  
  errores <- !is.na(texto) & (is.na(numero) | !is.finite(numero))
  
  if (any(errores)) {
    ejemplos <- paste(
      head(unique(texto[errores]), 5),
      collapse = ", "
    )
    
    stop(paste0(
      "Revisa el formato de la columna '",
      nombre_columna,
      "'. Valores no válidos: ",
      ejemplos
    ))
  }
  
  return(numero)
}

columnas_numericas <- c(
  "bedrooms", "beds", "review_scores_rating", "price"
)

for (columna in columnas_numericas) {
  df[[columna]] <- convertir_numero(df[[columna]], columna)
}

print("Columnas numéricas convertidas correctamente.")

# ==============================================================================
# RETO 1: TRATAMIENTO DE VALORES NULOS
# ==============================================================================

print("RETO 1: Tratando valores nulos...")

# Limpiar los nombres de región
df <- df %>%
  mutate(
    region_origen = na_if(
      trimws(as.character(region_origen)),
      ""
    )
  )

# Devuelve NA si todos los valores del grupo están vacíos
mediana_segura <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  median(x, na.rm = TRUE)
}

# Rellenar habitaciones y camas con la mediana de cada región.
# Si no hay región o toda la región carece de datos, se conserva NA.
df <- df %>%
  group_by(region_origen) %>%
  mutate(
    bedrooms = if_else(
      is.na(bedrooms) & !is.na(region_origen),
      mediana_segura(bedrooms),
      bedrooms
    ),
    beds = if_else(
      is.na(beds) & !is.na(region_origen),
      mediana_segura(beds),
      beds
    )
  ) %>%
  ungroup()

# Conservar la bandera existente y señalar puntuaciones ausentes.
# Una puntuación rellenada con 0 no representa una valoración real.
if (!"has_no_reviews" %in% names(df)) {
  df$has_no_reviews <- as.integer(is.na(df$review_scores_rating))
} else {
  df$has_no_reviews <- ifelse(
    is.na(df$review_scores_rating),
    1,
    df$has_no_reviews
  )
}

df <- df %>%
  mutate(
    review_scores_rating = replace_na(review_scores_rating, 0),
    host_is_superhost = na_if(
      str_to_lower(str_trim(as.character(host_is_superhost))),
      ""
    )
  )

# Los Superhost desconocidos se conservan como NA:
# no tener información no equivale a no ser Superhost.

print(paste(
  "Nulos restantes en bedrooms:",
  sum(is.na(df$bedrooms))
))

print(paste(
  "Nulos restantes en beds:",
  sum(is.na(df$beds))
))

# ==============================================================================
# RETO 2: FILTRAR PRECIOS
# ==============================================================================

print("RETO 2: Aplicando el rango de precios de 10 a 1.000 euros...")

filas_antes_filtro <- nrow(df)

precios_nulos <- sum(is.na(df$price))

precios_fuera_rango <- sum(
  df$price < 10 | df$price > 1000,
  na.rm = TRUE
)

# Este rango es un criterio de análisis:
# un precio fuera del rango no es necesariamente un error.
df <- df %>%
  filter(!is.na(price), price >= 10, price <= 1000)

print(paste("Filas descartadas por precio nulo:", precios_nulos))
print(paste("Filas fuera del rango:", precios_fuera_rango))
print(paste("Filas conservadas:", nrow(df)))

# ==============================================================================
# RETO 3: COMPROBAR COORDENADAS
# ==============================================================================

# No se calcula bathrooms_num porque no existe bathrooms_text.

# Detectar coordenadas no válidas sin modificar las originales.
# Los decimales perdidos deben revisarse en la fuente original.
if (all(c("latitude", "longitude") %in% names(df))) {
  
  latitud <- suppressWarnings(
    as.numeric(as.character(df$latitude))
  )
  
  longitud <- suppressWarnings(
    as.numeric(as.character(df$longitude))
  )
  
  df$coordenadas_validas <- (
    !is.na(latitud) &
      !is.na(longitud) &
      is.finite(latitud) &
      is.finite(longitud) &
      abs(latitud) <= 90 &
      abs(longitud) <= 180
  )
  
  print(paste(
    "Filas con coordenadas ausentes o fuera de rango:",
    sum(!df$coordenadas_validas)
  ))
}

# ==============================================================================
# 4. GUARDAR EL RESULTADO
# ==============================================================================

archivo_final <- file.path(
  dirname(ruta),
  "airbnb_benelux_preprocesado.csv"
)

# CSV con coma separadora y punto decimal
write_csv(df, archivo_final, na = "")

print("Preprocesamiento finalizado.")
print(paste("Filas iniciales:", filas_iniciales))
print(paste("Filas finales:", nrow(df)))
print(paste("Archivo guardado en:", archivo_final))

# Mostrar el resultado
print(head(df))

print(
  df %>%
    summarise(
      filas = n(),
      nulos_bedrooms = sum(is.na(bedrooms)),
      nulos_beds = sum(is.na(beds)),
      precio_medio = mean(price, na.rm = TRUE)
    )
)
