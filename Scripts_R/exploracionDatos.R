# ==============================================================================
# AIRBNB BENELUX — SCRIPT COMPLETO
# CARGA + PREPARACIÓN + EJERCICIOS 1–20 + CUADRO DE MANDO
# ==============================================================================

# CONFIGURACIÓN ---------------------------------------------------------------

# Déjala vacía para seleccionar el archivo la primera vez.
# También puedes escribir la ruta completa usando barras /.
RUTA <- ""

# Ejecutar todos los ejercicios:
EJERCICIOS <- 1:20

# Solo se utilizan si seleccionas un CSV:
SEPARADOR_CSV <- ","

# Separador decimal de los datos: "." para 100.50; "," para 100,50.
DECIMAL <- "."

# Hoja del Excel que contiene los datos:
HOJA_EXCEL <- 1

# ==============================================================================
# 1. INSTALAR Y CARGAR PAQUETES
# ==============================================================================

paquetes <- c(
  "tidyverse",
  "readxl",
  "plotly",
  "DT",
  "htmlwidgets",
  "htmltools",
  "leaflet",
  "servr"
)

faltan_paquetes <- paquetes[
  !vapply(
    paquetes,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(faltan_paquetes) > 0) {
  install.packages(faltan_paquetes)
}

if (!all(vapply(
  paquetes,
  requireNamespace,
  logical(1),
  quietly = TRUE
))) {
  stop("Faltan paquetes. Revisa los mensajes de instalación.")
}

suppressPackageStartupMessages(library(tidyverse))

# ==============================================================================
# 2. CARGAR EL ARCHIVO
# ==============================================================================

# Guardar la ruta junto al script, si se ejecuta mediante Source.
fichero_script <- tryCatch(
  sys.frame(1)$ofile,
  error = function(e) NULL
)

carpeta_config <- if (!is.null(fichero_script)) {
  dirname(normalizePath(fichero_script))
} else {
  getwd()
}

archivo_config <- file.path(
  carpeta_config,
  "airbnb_ruta_guardada.rds"
)

if (!nzchar(RUTA) && file.exists(archivo_config)) {
  RUTA <- readRDS(archivo_config)
}

if (!nzchar(RUTA) || !file.exists(RUTA)) {
  message("Selecciona tu archivo Excel o CSV con los alojamientos.")
  RUTA <- file.choose()
}

saveRDS(RUTA, archivo_config)

extension <- tolower(tools::file_ext(RUTA))

if (extension %in% c("xls", "xlsx")) {
  
  df <- readxl::read_excel(
    RUTA,
    sheet = HOJA_EXCEL,
    col_types = "text"
  )
  
} else if (extension == "csv") {
  
  df <- readr::read_delim(
    RUTA,
    delim = SEPARADOR_CSV,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )
  
} else {
  stop("Selecciona un archivo XLS, XLSX o CSV.")
}

if (nrow(df) == 0) {
  stop("El archivo o la hoja seleccionada no contiene datos.")
}

columnas_necesarias <- c(
  "id", "name", "host_id", "host_name",
  "host_is_superhost", "region_origen", "room_type",
  "neighbourhood_cleansed", "price", "accommodates",
  "bedrooms", "review_scores_rating", "number_of_reviews"
)

faltantes <- setdiff(columnas_necesarias, names(df))

if (length(faltantes) > 0) {
  stop(paste(
    "Faltan columnas:",
    paste(faltantes, collapse = ", "),
    "\nSi todo aparece en una columna, revisa SEPARADOR_CSV."
  ))
}

# ==============================================================================
# 3. CONVERTIR COLUMNAS A NÚMEROS
# ==============================================================================

convertir_numero <- function(x, nombre) {
  
  texto <- trimws(as.character(x))
  
  texto[
    toupper(texto) %in% c("", "NA", "N/A", "NULL", "NAN")
  ] <- NA_character_
  
  texto <- gsub("[$€£[:space:]]", "", texto)
  
  if (DECIMAL == ",") {
    texto <- gsub(".", "", texto, fixed = TRUE)
    texto <- gsub(",", ".", texto, fixed = TRUE)
  } else {
    texto <- gsub(",", "", texto, fixed = TRUE)
  }
  
  valor <- suppressWarnings(as.numeric(texto))
  
  errores <- !is.na(texto) & !is.finite(valor)
  
  if (any(errores)) {
    stop(paste(
      "Formato numérico no válido en",
      nombre,
      ". Ejemplos:",
      paste(head(unique(texto[errores]), 5), collapse = ", ")
    ))
  }
  
  valor
}

columnas_numericas <- intersect(
  c(
    "price", "accommodates", "bedrooms", "beds",
    "review_scores_rating", "number_of_reviews"
  ),
  names(df)
)

for (columna in columnas_numericas) {
  df[[columna]] <- convertir_numero(
    df[[columna]],
    columna
  )
}

df <- df %>%
  mutate(
    across(
      where(is.character),
      ~ na_if(trimws(.x), "")
    ),
    host_is_superhost = case_when(
      tolower(host_is_superhost) %in%
        c("t", "true", "1", "sí", "si") ~ "t",
      
      tolower(host_is_superhost) %in%
        c("f", "false", "0", "no") ~ "f",
      
      TRUE ~ NA_character_
    )
  )

carpeta_salida <- file.path(
  dirname(RUTA),
  "resultados_EDA_R"
)

carpeta_4k <- file.path(
  carpeta_salida,
  "analisis_avanzado_4K"
)

dir.create(
  carpeta_4k,
  recursive = TRUE,
  showWarnings = FALSE
)

message("Datos cargados: ", nrow(df), " filas.")

# ==============================================================================
# 4. FUNCIONES COMUNES
# ==============================================================================

T <- htmltools::tags
resultados <- list()

media_segura <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

euros <- function(x) {
  ifelse(
    is.finite(x),
    paste0(
      formatC(
        x,
        format = "f",
        digits = 2,
        decimal.mark = ",",
        big.mark = "."
      ),
      " €"
    ),
    "Sin datos"
  )
}

escapar <- function(x) {
  as.character(
    htmltools::htmlEscape(
      ifelse(is.na(x), "Sin datos", as.character(x))
    )
  )
}

guardar_tabla <- function(datos, numero) {
  write_csv(
    as_tibble(datos),
    file.path(
      carpeta_salida,
      sprintf("ej%02d_resultados.csv", numero)
    ),
    na = ""
  )
}

registrar <- function(numero, titulo, contenido = NULL, nota = "") {
  resultados[[as.character(numero)]] <<- list(
    titulo = titulo,
    contenido = contenido,
    nota = nota
  )
  message("Ejercicio ", numero, " preparado.")
}

tabla <- function(datos) {
  
  DT::datatable(
    datos,
    rownames = FALSE,
    filter = "top",
    extensions = "Buttons",
    class = "compact stripe hover",
    options = list(
      pageLength = 8,
      scrollX = TRUE,
      dom = "Bfrtip",
      buttons = c("copy", "csv"),
      language = list(
        search = "Buscar:",
        zeroRecords = "Sin coincidencias",
        info = "_START_–_END_ de _TOTAL_ registros",
        infoEmpty = "Sin registros",
        paginate = list(
          previous = "Anterior",
          "next" = "Siguiente"
        )
      )
    )
  )
}

estilo <- function(p, eje_x = "", alto = 440, margen = 175) {
  
  p <- plotly::layout(
    p,
    height = alto,
    paper_bgcolor = "white",
    plot_bgcolor = "white",
    font = list(
      family = "Arial",
      color = "#334155"
    ),
    margin = list(
      l = margen,
      r = 55,
      t = 40,
      b = 85
    ),
    xaxis = list(
      title = eje_x,
      gridcolor = "#E8EDF3",
      automargin = TRUE
    ),
    yaxis = list(
      title = "",
      automargin = TRUE
    ),
    legend = list(
      orientation = "h",
      y = -0.22
    )
  )
  
  plotly::config(
    p,
    displaylogo = FALSE,
    responsive = TRUE
  )
}

positivos <- df %>%
  filter(is.finite(price), price > 0)

# ==============================================================================
# EJERCICIO 1: PODIO DE PRECIOS
# ==============================================================================

if (1 %in% EJERCICIOS) {
  
  d <- positivos %>%
    arrange(desc(price)) %>%
    slice_head(n = 5)
  
  guardar_tabla(d, 1)
  
  colores <- c(
    "#4F46E5", "#0D9488", "#8B5CF6",
    "#2563EB", "#DB2777"
  )
  
  tarjetas <- lapply(seq_len(nrow(d)), function(i) {
    T$div(
      class = "mini",
      style = paste0("border-top:5px solid ", colores[i], ";"),
      T$strong(paste0("#", i)),
      T$p(d$region_origen[i]),
      T$h3(coalesce(d$name[i], "Sin nombre")),
      T$div(class = "cifra", euros(d$price[i])),
      T$small("por noche")
    )
  })
  
  registrar(
    1,
    "Top 5 de precios por noche",
    T$div(class = "cards", htmltools::tagList(tarjetas)),
    "Mayor precio no significa mayor calidad. Revisa los precios extremos en la fuente."
  )
}

# ==============================================================================
# EJERCICIO 2: VOLUMEN Y PRECIO
# ==============================================================================

if (2 %in% EJERCICIOS) {
  
  d <- df %>%
    filter(!is.na(region_origen)) %>%
    group_by(region_origen) %>%
    summarise(
      Alojamientos = n(),
      Precio = media_segura(price[price > 0]),
      .groups = "drop"
    ) %>%
    arrange(Alojamientos)
  
  guardar_tabla(d, 2)
  
  p1 <- plotly::plot_ly(
    d,
    x = ~Alojamientos,
    y = ~region_origen,
    type = "bar",
    orientation = "h",
    marker = list(color = "#4F46E5"),
    showlegend = FALSE
  )
  
  p2 <- plotly::plot_ly(
    d,
    x = ~Precio,
    y = ~region_origen,
    type = "bar",
    orientation = "h",
    marker = list(color = "#14B8A6"),
    showlegend = FALSE
  )
  
  contenido <- T$div(
    T$h3("Número de alojamientos"),
    estilo(p1, "Alojamientos", alto = 320),
    T$h3("Precio medio"),
    estilo(p2, "Precio por noche (€)", alto = 320)
  )
  
  registrar(2, "Tamaño y precio del mercado", contenido)
}

# ==============================================================================
# EJERCICIO 3: SUPERHOST
# ==============================================================================

if (3 %in% EJERCICIOS) {
  
  d <- df %>%
    filter(
      review_scores_rating > 0,
      review_scores_rating <= 5,
      host_is_superhost %in% c("f", "t")
    ) %>%
    group_by(host_is_superhost) %>%
    summarise(
      Precio = media_segura(price[price > 0]),
      Nota = mean(review_scores_rating),
      Alojamientos = n(),
      .groups = "drop"
    )
  
  guardar_tabla(d, 3)
  
  tarjetas <- lapply(seq_len(nrow(d)), function(i) {
    T$div(
      class = "mini",
      T$h3(ifelse(
        d$host_is_superhost[i] == "t",
        "Superhost",
        "Otros anfitriones"
      )),
      T$p("Precio medio"),
      T$strong(class = "cifra", euros(d$Precio[i])),
      T$p(sprintf("Nota: %.2f / 5", d$Nota[i])),
      T$p(paste("Alojamientos:", d$Alojamientos[i]))
    )
  })
  
  registrar(
    3,
    "Superhost frente a otros anfitriones",
    T$div(class = "cards", htmltools::tagList(tarjetas)),
    "Solo alojamientos con nota válida. Comparación descriptiva, no causal."
  )
}

# ==============================================================================
# EJERCICIO 4: SELECCIÓN EN ÁMSTERDAM
# ==============================================================================

if (4 %in% EJERCICIOS) {
  
  d <- df %>%
    filter(
      region_origen == "Amsterdam (Holanda)",
      room_type == "Entire home/apt",
      price > 80,
      price < 200,
      accommodates >= 4,
      bedrooms >= 3,
      review_scores_rating >= 4
    ) %>%
    transmute(
      Alojamiento = name,
      Ciudad = region_origen,
      Precio = price,
      Capacidad = accommodates,
      Dormitorios = bedrooms,
      Nota = review_scores_rating
    ) %>%
    arrange(Precio)
  
  guardar_tabla(d, 4)
  
  w <- tabla(d) %>%
    DT::formatCurrency(
      "Precio",
      currency = " €",
      before = FALSE,
      mark = ".",
      dec.mark = ","
    ) %>%
    DT::formatRound("Nota", digits = 2) %>%
    DT::formatStyle(
      "Precio",
      backgroundColor = DT::styleInterval(
        c(120, 160),
        c("#CCFBF1", "#DBEAFE", "#EDE9FE")
      ),
      color = "#17324D",
      fontWeight = "bold"
    )
  
  registrar(4, "Selección de alojamientos en Ámsterdam", w)
}

# ==============================================================================
# EJERCICIO 5: TIPOLOGÍAS POR CIUDAD
# ==============================================================================

if (5 %in% EJERCICIOS) {
  
  d <- df %>%
    filter(!is.na(region_origen), !is.na(room_type)) %>%
    mutate(
      Tipo = recode(
        room_type,
        "Entire home/apt" = "Vivienda completa",
        "Private room" = "Habitación privada",
        "Shared room" = "Habitación compartida",
        "Hotel room" = "Habitación de hotel"
      )
    ) %>%
    count(region_origen, Tipo, name = "Total")
  
  guardar_tabla(d, 5)
  
  p <- plotly::plot_ly(
    d,
    x = ~Total,
    y = ~region_origen,
    color = ~Tipo,
    colors = c("#4F46E5", "#14B8A6", "#A78BFA", "#F472B6"),
    type = "bar",
    orientation = "h",
    text = ~Total,
    textposition = "auto"
  )
  
  p <- plotly::layout(p, barmode = "group")
  
  registrar(
    5,
    "Tipos de alojamiento por ciudad",
    estilo(p, "Número de alojamientos", alto = 600),
    "Pulsa las categorías de la leyenda para ocultarlas o mostrarlas."
  )
}

# ==============================================================================
# EJERCICIO 6: RANKINGS DE RESEÑAS CON PUNTOS
# ==============================================================================

if (6 %in% EJERCICIOS) {
  
  mayores <- df %>%
    filter(is.finite(number_of_reviews), number_of_reviews > 0) %>%
    arrange(desc(number_of_reviews)) %>%
    slice_head(n = 10)
  
  menores <- df %>%
    filter(is.finite(number_of_reviews), number_of_reviews > 250) %>%
    arrange(number_of_reviews) %>%
    slice_head(n = 10)
  
  guardar_tabla(
    bind_rows(
      mutate(mayores, Grupo = "Más reseñas"),
      mutate(menores, Grupo = "Menos entre los que superan 250")
    ),
    6
  )
  
  crear_puntos <- function(d, color) {
    
    if (nrow(d) == 0) return(T$p("Sin resultados."))
    
    d$etiqueta <- paste0(
      seq_len(nrow(d)),
      ". ",
      str_trunc(coalesce(d$name, "Sin nombre"), 30)
    )
    
    d$detalle <- paste0(
      escapar(d$name),
      "<br>Ciudad: ", escapar(d$region_origen),
      "<br>Reseñas: ", d$number_of_reviews,
      "<br>Nota: ", d$review_scores_rating,
      "<br>Dormitorios: ", d$bedrooms
    )
    
    p <- plotly::plot_ly(d) %>%
      plotly::add_segments(
        x = 0,
        xend = ~number_of_reviews,
        y = ~etiqueta,
        yend = ~etiqueta,
        line = list(color = "#DFE7F1", width = 5),
        hoverinfo = "skip",
        showlegend = FALSE
      ) %>%
      plotly::add_trace(
        x = ~number_of_reviews,
        y = ~etiqueta,
        type = "scatter",
        mode = "markers+text",
        marker = list(color = color, size = 13),
        text = ~number_of_reviews,
        textposition = "middle right",
        customdata = ~detalle,
        hovertemplate = "%{customdata}<extra></extra>",
        showlegend = FALSE
      )
    
    p <- plotly::layout(
      p,
      yaxis = list(
        categoryorder = "array",
        categoryarray = rev(d$etiqueta)
      ),
      xaxis = list(
        range = c(0, max(d$number_of_reviews) * 1.25)
      )
    )
    
    estilo(p, "Número de reseñas", alto = 480, margen = 235)
  }
  
  registrar(
    6,
    "Rankings de reseñas",
    T$div(
      T$h3("Diez con más reseñas"),
      crear_puntos(mayores, "#4F46E5"),
      T$details(
        T$summary("Ver diez con menos entre los que superan 250"),
        crear_puntos(menores, "#14B8A6")
      )
    ),
    "Los dos rankings tienen escalas independientes."
  )
}

# ==============================================================================
# EJERCICIO 7: BARRIOS
# ==============================================================================

if (7 %in% EJERCICIOS) {
  
  d <- df %>%
    filter(
      !is.na(region_origen),
      !is.na(neighbourhood_cleansed)
    ) %>%
    group_by(region_origen, neighbourhood_cleansed) %>%
    summarise(
      Alojamientos = n(),
      Nota = media_segura(
        review_scores_rating[
          review_scores_rating > 0 &
            review_scores_rating <= 5
        ]
      ),
      Dormitorios = media_segura(bedrooms),
      .groups = "drop"
    ) %>%
    arrange(desc(Alojamientos)) %>%
    slice_head(n = 10) %>%
    rename(
      Ciudad = region_origen,
      Barrio = neighbourhood_cleansed
    )
  
  guardar_tabla(d, 7)
  
  w <- tabla(d) %>%
    DT::formatRound(c("Nota", "Dormitorios"), digits = 2)
  
  if (nrow(d) > 0) {
    w <- w %>%
      DT::formatStyle(
        "Alojamientos",
        background = DT::styleColorBar(
          c(0, max(d$Alojamientos)),
          "#BFDBFE"
        ),
        backgroundSize = "95% 70%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  }
  
  registrar(7, "Barrios con más alojamientos", w)
}

# ==============================================================================
# EJERCICIO 8: TARJETAS PARA GRUPOS
# ==============================================================================

if (8 %in% EJERCICIOS) {
  
  d <- positivos %>%
    filter(
      room_type == "Entire home/apt",
      accommodates >= 6,
      price < 150
    ) %>%
    mutate(precio_persona = price / accommodates) %>%
    arrange(precio_persona) %>%
    slice_head(n = 6)
  
  guardar_tabla(d, 8)
  
  tarjetas <- lapply(seq_len(nrow(d)), function(i) {
    T$div(
      class = "mini",
      T$small(d$region_origen[i]),
      T$h3(coalesce(d$name[i], "Sin nombre")),
      T$div(class = "cifra", euros(d$precio_persona[i])),
      T$p("por persona y noche"),
      T$p(paste("Total por noche:", euros(d$price[i]))),
      T$p(paste("Capacidad:", d$accommodates[i], "personas")),
      T$p(paste("Dormitorios:", d$bedrooms[i]))
    )
  })
  
  registrar(
    8,
    "Seis opciones económicas para grupos",
    T$div(class = "cards", htmltools::tagList(tarjetas)),
    "Precio por persona a ocupación completa. No incluye cargos adicionales no recogidos en price."
  )
}

# ==============================================================================
# EJERCICIO 9: DISTRIBUCIÓN DE PRECIOS
# ==============================================================================

if (9 %in% EJERCICIOS) {
  
  p <- plotly::plot_ly(
    positivos,
    x = ~price,
    y = ~region_origen,
    color = ~region_origen,
    type = "box",
    orientation = "h",
    boxpoints = FALSE,
    showlegend = FALSE
  )
  
  d <- positivos %>%
    group_by(region_origen) %>%
    summarise(
      Minimo = min(price),
      Mediana = median(price),
      Maximo = max(price),
      .groups = "drop"
    )
  
  guardar_tabla(d, 9)
  
  registrar(
    9,
    "Distribución de precios por ciudad",
    T$div(
      estilo(p, "Precio por noche (€)"),
      T$details(
        T$summary("Consultar mínimos, medianas y máximos"),
        tabla(d)
      )
    ),
    "Se ocultan los puntos extremos, pero no se eliminan del cálculo."
  )
}

# ==============================================================================
# EJERCICIO 10: CONCENTRACIÓN EN EL TOP 10 DE ANFITRIONES
# ==============================================================================

if (10 %in% EJERCICIOS) {
  
  base_hosts <- df %>%
    filter(!is.na(host_id), !is.na(id)) %>%
    distinct(host_id, id, .keep_all = TRUE)
  
  d <- base_hosts %>%
    group_by(host_id) %>%
    summarise(
      Nombre = first(coalesce(host_name, "Sin nombre")),
      Alojamientos = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(Alojamientos)) %>%
    slice_head(n = 10) %>%
    mutate(
      acumulado = 100 * cumsum(Alojamientos) / nrow(base_hosts),
      etiqueta = paste0(
        row_number(), ". ",
        str_trunc(Nombre, 18)
      )
    )
  
  guardar_tabla(d, 10)
  
  if (nrow(d) > 0) {
    
    p <- plotly::plot_ly(
      d,
      x = ~etiqueta,
      y = ~Alojamientos,
      type = "bar",
      name = "Alojamientos",
      marker = list(color = "#4F46E5")
    ) %>%
      plotly::add_trace(
        x = ~etiqueta,
        y = ~acumulado,
        type = "scatter",
        mode = "lines+markers",
        yaxis = "y2",
        name = "Cuota acumulada (%)",
        line = list(color = "#14B8A6"),
        inherit = FALSE
      )
    
    p <- estilo(p, margen = 65)
    
    p <- plotly::layout(
      p,
      yaxis2 = list(
        overlaying = "y",
        side = "right",
        range = c(0, 100),
        title = "Cuota acumulada (%)",
        showgrid = FALSE
      ),
      xaxis = list(tickangle = -35),
      margin = list(l = 65, r = 75, t = 35, b = 120)
    )
    
    registrar(
      10,
      "Los mayores anfitriones",
      p,
      "La cuota acumulada utiliza todas las parejas anfitrión-alojamiento identificadas como denominador."
    )
    
  } else {
    registrar(10, "Los mayores anfitriones", nota = "Sin datos.")
  }
}

# ==============================================================================
# EJERCICIO 11: PRECIO REAL FRENTE A LA MEDIANA
# ==============================================================================

if (11 %in% EJERCICIOS) {
  
  d <- positivos %>%
    filter(!is.na(region_origen), !is.na(room_type)) %>%
    group_by(region_origen, room_type) %>%
    mutate(
      referencia = median(price),
      descuento = 100 * (1 - price / referencia)
    ) %>%
    ungroup() %>%
    filter(
      descuento >= 30,
      review_scores_rating >= 4.8,
      review_scores_rating <= 5,
      number_of_reviews >= 50
    ) %>%
    arrange(desc(descuento)) %>%
    slice_head(n = 8) %>%
    mutate(
      etiqueta = paste0(
        row_number(), ". ",
        str_trunc(coalesce(name, "Sin nombre"), 27)
      )
    )
  
  guardar_tabla(d, 11)
  
  if (nrow(d) > 0) {
    
    p <- plotly::plot_ly(d) %>%
      plotly::add_segments(
        x = ~price,
        xend = ~referencia,
        y = ~etiqueta,
        yend = ~etiqueta,
        line = list(color = "#CBD5E1", width = 5),
        showlegend = FALSE,
        hoverinfo = "skip"
      ) %>%
      plotly::add_markers(
        x = ~price,
        y = ~etiqueta,
        name = "Precio real",
        marker = list(color = "#0D9488", size = 12)
      ) %>%
      plotly::add_trace(
        x = ~referencia,
        y = ~etiqueta,
        type = "scatter",
        mode = "markers+text",
        name = "Mediana",
        marker = list(
          color = "#8B5CF6",
          size = 11,
          symbol = "diamond"
        ),
        text = ~paste0("−", round(descuento), "%"),
        textposition = "middle right",
        cliponaxis = FALSE
      )
    
    p <- plotly::layout(
      p,
      yaxis = list(
        categoryorder = "array",
        categoryarray = rev(d$etiqueta)
      ),
      xaxis = list(
        range = c(0, max(d$referencia) * 1.25)
      )
    )
    
    registrar(
      11,
      "Precio real frente a referencia",
      estilo(p, "Precio por noche (€)", margen = 230),
      "Referencia por ciudad y tipo, sin ajustar capacidad o barrio. No es una rebaja anunciada."
    )
    
  } else {
    registrar(
      11,
      "Precio real frente a referencia",
      nota = "No hay casos que cumplan todos los criterios."
    )
  }
}

# ==============================================================================
# EJERCICIO 12: COMPOSICIÓN PORCENTUAL
# ==============================================================================

if (12 %in% EJERCICIOS) {
  
  d <- df %>%
    filter(!is.na(region_origen), !is.na(room_type)) %>%
    count(region_origen, room_type) %>%
    group_by(region_origen) %>%
    mutate(porcentaje = 100 * n / sum(n)) %>%
    ungroup()
  
  guardar_tabla(d, 12)
  
  p <- plotly::plot_ly(
    d,
    x = ~porcentaje,
    y = ~region_origen,
    color = ~room_type,
    colors = c("#4F46E5", "#14B8A6", "#A78BFA", "#F472B6"),
    type = "bar",
    orientation = "h",
    text = ~ifelse(
      porcentaje >= 7,
      paste0(round(porcentaje), "%"),
      ""
    ),
    textposition = "inside"
  )
  
  p <- plotly::layout(
    p,
    barmode = "stack",
    xaxis = list(range = c(0, 100), ticksuffix = "%")
  )
  
  registrar(
    12,
    "Composición del mercado",
    estilo(p, "Porcentaje de alojamientos")
  )
}

# ==============================================================================
# EJERCICIO 13: PRECIO, NOTA Y RESEÑAS EN 3D
# ==============================================================================

if (13 %in% EJERCICIOS) {
  
  d <- positivos %>%
    filter(
      review_scores_rating > 0,
      review_scores_rating <= 5,
      number_of_reviews >= 20,
      !is.na(region_origen)
    )
  
  total_13 <- nrow(d)
  
  set.seed(123)
  
  if (nrow(d) > 2000) {
    d <- d %>%
      group_by(region_origen) %>%
      slice_sample(prop = 2000 / total_13) %>%
      ungroup()
  }
  
  guardar_tabla(d, 13)
  
  if (nrow(d) > 0) {
    
    p <- plotly::plot_ly(
      d,
      x = ~price,
      y = ~review_scores_rating,
      z = ~number_of_reviews,
      color = ~region_origen,
      colors = c(
        "#4F46E5", "#14B8A6", "#A78BFA",
        "#F472B6", "#38BDF8"
      ),
      type = "scatter3d",
      mode = "markers",
      marker = list(size = 3, opacity = 0.7),
      text = ~paste0(escapar(name), "<br>", escapar(region_origen)),
      hovertemplate = paste0(
        "%{text}<br>Precio: %{x:.2f} €",
        "<br>Nota: %{y}<br>Reseñas: %{z}<extra></extra>"
      )
    )
    
    p <- plotly::layout(
      p,
      height = 550,
      margin = list(l = 0, r = 0, t = 15, b = 70),
      legend = list(orientation = "h"),
      scene = list(
        xaxis = list(title = "Precio (€), escala log", type = "log"),
        yaxis = list(title = "Nota"),
        zaxis = list(title = "Reseñas"),
        aspectmode = "cube"
      )
    )
    
    registrar(
      13,
      "Precio, nota y reseñas en 3D",
      plotly::config(p, displaylogo = FALSE),
      paste(
        nrow(d), "de", total_13,
        "casos válidos representados. Arrastra para girar.",
        "Muestreo reproducible si hay más de 2.000 casos."
      )
    )
    
  } else {
    registrar(13, "Exploración 3D", nota = "Sin datos válidos.")
  }
}

# ==============================================================================
# EJERCICIO 14: MAPA DE CALOR SUPERHOST
# ==============================================================================

if (14 %in% EJERCICIOS) {
  
  d <- positivos %>%
    filter(
      !is.na(region_origen),
      !is.na(room_type),
      host_is_superhost %in% c("f", "t")
    ) %>%
    group_by(region_origen, room_type, host_is_superhost) %>%
    summarise(
      precio = mean(price),
      muestra = n(),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = host_is_superhost,
      values_from = c(precio, muestra)
    )
  
  for (columna in c("precio_f", "precio_t")) {
    if (!columna %in% names(d)) {
      d[[columna]] <- rep(NA_real_, nrow(d))
    }
  }
  
  d <- d %>%
    mutate(diferencia = 100 * (precio_t / precio_f - 1))
  
  guardar_tabla(d, 14)
  
  if (any(is.finite(d$diferencia))) {
    
    ciudades <- sort(unique(d$region_origen))
    tipos <- sort(unique(d$room_type))
    
    matriz <- matrix(
      NA_real_,
      nrow = length(ciudades),
      ncol = length(tipos),
      dimnames = list(ciudades, tipos)
    )
    
    for (i in seq_len(nrow(d))) {
      matriz[d$region_origen[i], d$room_type[i]] <- d$diferencia[i]
    }
    
    limite <- max(abs(matriz), na.rm = TRUE)
    if (limite == 0) limite <- 1
    
    p <- plotly::plot_ly(
      x = colnames(matriz),
      y = rownames(matriz),
      z = unname(matriz),
      type = "heatmap",
      zmin = -limite,
      zmax = limite,
      colorscale = list(
        list(0, "#99F6E4"),
        list(0.5, "#F8FAFC"),
        list(1, "#C4B5FD")
      ),
      hoverongaps = FALSE,
      hovertemplate = paste0(
        "%{y}<br>%{x}",
        "<br>Diferencia Superhost: %{z:.1f}%",
        "<extra></extra>"
      ),
      colorbar = list(title = "%")
    )
    
    registrar(
      14,
      "Diferencia de precio Superhost",
      estilo(p, alto = 420),
      "Turquesa: más barato; lavanda: más caro. Diferencia respecto a otros anfitriones de la misma ciudad y tipología; no ajusta capacidad ni barrio."
    )
    
  } else {
    registrar(
      14,
      "Diferencia de precio Superhost",
      nota = "No hay comparaciones completas disponibles."
    )
  }
}

# ==============================================================================
# EJERCICIO 15: MAPA INTERACTIVO
# ==============================================================================

if (15 %in% EJERCICIOS) {
  
  if (all(c("latitude", "longitude") %in% names(df))) {
    
    reconstruir <- function(x, posiciones) {
      
      texto <- gsub(
        ",", ".",
        trimws(as.character(x)),
        fixed = TRUE
      )
      
      valor <- suppressWarnings(as.numeric(texto))
      
      enteros <- !is.na(texto) &
        grepl("^[0-9]+$", texto) &
        nchar(texto) > posiciones
      
      valores <- texto[enteros]
      
      valor[enteros] <- as.numeric(
        paste0(
          substr(valores, 1, posiciones),
          ".",
          substring(valores, posiciones + 1)
        )
      )
      
      valor
    }
    
    dm <- df %>%
      mutate(
        lat = reconstruir(latitude, 2),
        lon = reconstruir(longitude, 1)
      ) %>%
      filter(
        is.finite(lat),
        is.finite(lon),
        between(lat, 49, 54),
        between(lon, 2, 8)
      )
    
    message(
      "Mapa: ", nrow(dm),
      " alojamientos; excluidos: ", nrow(df) - nrow(dm)
    )
    
    if (nrow(dm) > 0) {
      
      dm$ficha <- paste0(
        "<b>", escapar(dm$name), "</b>",
        "<br>Ciudad: ", escapar(dm$region_origen),
        "<br>Tipo: ", escapar(dm$room_type),
        "<br>Precio: ", euros(dm$price),
        "<br>Capacidad: ", dm$accommodates,
        "<br>Dormitorios: ", dm$bedrooms,
        "<br>Nota: ", dm$review_scores_rating,
        "<br>Reseñas: ", dm$number_of_reviews
      )
      
      mapa <- leaflet::leaflet(dm) %>%
        leaflet::addTiles(
          urlTemplate = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
          attribution = "&copy; OpenStreetMap contributors"
        ) %>%
        leaflet::addMarkers(
          lng = ~lon,
          lat = ~lat,
          popup = ~ficha,
          clusterOptions = leaflet::markerClusterOptions(
            chunkedLoading = TRUE
          )
        )
      
      if (
        diff(range(dm$lon)) == 0 &&
        diff(range(dm$lat)) == 0
      ) {
        
        mapa <- leaflet::setView(
          mapa,
          lng = dm$lon[1],
          lat = dm$lat[1],
          zoom = 13
        )
        
      } else {
        
        mapa <- leaflet::fitBounds(
          mapa,
          min(dm$lon), min(dm$lat),
          max(dm$lon), max(dm$lat)
        )
      }
      
      htmlwidgets::saveWidget(
        mapa,
        file.path(
          carpeta_salida,
          "ej15_mapa_airbnb_actualizado.html"
        ),
        selfcontained = FALSE
      )
      
      registrar(
        15,
        "Mapa de alojamientos",
        T$iframe(
          src = "ej15_mapa_airbnb_actualizado.html",
          style = "width:100%;height:650px;border:0;"
        ),
        paste(
          "Coordenadas reconstruidas provisionalmente:",
          "contrástalas con la fuente original.",
          "El fondo necesita conexión a Internet."
        )
      )
      
    } else {
      registrar(15, "Mapa", nota = "No hay coordenadas válidas.")
    }
    
  } else {
    registrar(15, "Mapa", nota = "Faltan latitude o longitude.")
  }
}

# ==============================================================================
# PREPARACIÓN DE LOS ANÁLISIS AVANZADOS
# ==============================================================================

base_avanzada <- positivos %>%
  filter(
    !is.na(id),
    !is.na(region_origen),
    !is.na(room_type)
  ) %>%
  distinct(id, .keep_all = TRUE)

tema_4k <- function() {
  
  theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(
        fill = "#F5F7FB",
        colour = NA
      ),
      panel.background = element_rect(
        fill = "white",
        colour = NA
      ),
      plot.title = element_text(
        face = "bold",
        size = 23,
        colour = "#17324D"
      ),
      plot.subtitle = element_text(
        size = 12,
        colour = "#64748B"
      ),
      plot.caption = element_text(size = 9, hjust = 0),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.margin = margin(25, 25, 20, 25)
    )
}

exportar_4k <- function(grafico, numero, titulo, nota = "") {
  
  nombre <- sprintf("ej%02d_4K.png", numero)
  
  # 12,8 × 7,2 pulgadas a 300 dpi = 3840 × 2160 píxeles.
  ggsave(
    file.path(carpeta_4k, nombre),
    grafico,
    width = 12.8,
    height = 7.2,
    units = "in",
    dpi = 300,
    bg = "#F5F7FB"
  )
  
  ruta_relativa <- paste0(
    "analisis_avanzado_4K/",
    nombre
  )
  
  registrar(
    numero,
    titulo,
    T$a(
      href = ruta_relativa,
      target = "_blank",
      T$img(
        src = ruta_relativa,
        style = "width:100%;height:auto;",
        alt = titulo
      )
    ),
    nota
  )
}

# ==============================================================================
# EJERCICIO 16: PRECIO MEDIANO E INTERVALOS BOOTSTRAP
# ==============================================================================

if (16 %in% EJERCICIOS) {
  
  grupos <- base_avanzada %>%
    count(region_origen, room_type) %>%
    filter(n >= 30)
  
  d <- base_avanzada %>%
    semi_join(
      grupos,
      by = c("region_origen", "room_type")
    )
  
  if (nrow(d) > 0) {
    
    set.seed(1601)
    
    resumen <- d %>%
      group_by(region_origen, room_type) %>%
      group_modify(~ {
        
        precios <- .x$price
        
        medianas <- replicate(
          1000,
          median(
            sample(
              precios,
              length(precios),
              replace = TRUE
            )
          )
        )
        
        tibble(
          casos = length(precios),
          mediana = median(precios),
          inferior = unname(quantile(medianas, 0.025)),
          superior = unname(quantile(medianas, 0.975))
        )
      }) %>%
      ungroup()
    
    guardar_tabla(resumen, 16)
    
    g <- ggplot(
      resumen,
      aes(mediana, region_origen)
    ) +
      geom_segment(
        aes(
          x = inferior,
          xend = superior,
          yend = region_origen
        ),
        colour = "#A5B4FC",
        linewidth = 2
      ) +
      geom_point(colour = "#4F46E5", size = 3) +
      facet_wrap(
        ~room_type,
        ncol = 2,
        scales = "free_x"
      ) +
      labs(
        title = "Precio habitual e incertidumbre",
        subtitle = "Mediana e intervalo bootstrap del 95 % · mínimo 30 casos por grupo",
        x = "Precio por noche (€)",
        y = NULL,
        caption = paste(
          "1.000 remuestreos. Escalas independientes.",
          "El intervalo no recoge sesgos de cobertura del dataset."
        )
      ) +
      tema_4k()
    
    exportar_4k(
      g, 16,
      "Precio mediano e incertidumbre",
      "Pulsa la imagen para abrirla en 4K."
    )
    
  } else {
    registrar(
      16,
      "Precio mediano e incertidumbre",
      nota = "No hay grupos con al menos 30 casos."
    )
  }
}

# ==============================================================================
# EJERCICIO 17: CONCENTRACIÓN DE ANFITRIONES
# ==============================================================================

if (17 %in% EJERCICIOS) {
  
  hosts <- base_avanzada %>%
    filter(!is.na(host_id)) %>%
    count(region_origen, host_id, name = "alojamientos") %>%
    group_by(region_origen) %>%
    mutate(cuota = alojamientos / sum(alojamientos)) %>%
    ungroup()
  
  if (nrow(hosts) > 0) {
    
    resumen <- hosts %>%
      group_by(region_origen) %>%
      summarise(
        HHI = 10000 * sum(cuota^2),
        cuota_top10 = 100 * sum(
          head(sort(cuota, decreasing = TRUE), 10)
        ),
        anfitriones = n(),
        .groups = "drop"
      )
    
    guardar_tabla(resumen, 17)
    
    d <- resumen %>%
      pivot_longer(
        c(HHI, cuota_top10),
        names_to = "Metrica",
        values_to = "Valor"
      ) %>%
      mutate(
        Metrica = recode(
          Metrica,
          HHI = "Concentración HHI",
          cuota_top10 = "Cuota del top 10 (%)"
        )
      )
    
    g <- ggplot(
      d,
      aes(Valor, reorder(region_origen, Valor))
    ) +
      geom_col(fill = "#0D9488", width = 0.6) +
      geom_text(
        aes(label = round(Valor, 1)),
        hjust = -0.15,
        size = 4
      ) +
      facet_wrap(~Metrica, scales = "free_x") +
      scale_x_continuous(
        expand = expansion(mult = c(0, 0.2))
      ) +
      labs(
        title = "Concentración de la oferta por ciudad",
        subtitle = "Distribución de cuotas y peso de los mayores anfitriones",
        x = NULL,
        y = NULL,
        caption = paste(
          "HHI = suma de cuotas al cuadrado × 10.000.",
          "Mayor HHI indica mayor concentración en esta muestra."
        )
      ) +
      tema_4k()
    
    exportar_4k(g, 17, "Concentración de anfitriones")
    
  } else {
    registrar(
      17,
      "Concentración de anfitriones",
      nota = "No hay anfitriones identificados."
    )
  }
}

# ==============================================================================
# MODELO COMPARTIDO POR LOS EJERCICIOS 18 Y 20
# ==============================================================================

modelo <- NULL
validacion <- tibble()

if (any(c(18, 20) %in% EJERCICIOS)) {
  
  datos_modelo <- base_avanzada %>%
    filter(
      is.finite(accommodates),
      accommodates > 0,
      is.finite(bedrooms),
      bedrooms >= 0,
      review_scores_rating > 0,
      review_scores_rating <= 5,
      is.finite(number_of_reviews),
      number_of_reviews >= 0,
      host_is_superhost %in% c("f", "t")
    )
  
  if (nrow(datos_modelo) >= 100) {
    
    set.seed(1801)
    
    datos_modelo <- datos_modelo %>%
      group_by(
        region_origen,
        room_type,
        host_is_superhost
      ) %>%
      mutate(
        test = {
          indicador <- rep(FALSE, n())
          
          if (n() >= 4) {
            indicador[
              sample.int(n(), floor(n() * 0.25))
            ] <- TRUE
          }
          
          indicador
        }
      ) %>%
      ungroup()
    
    entrenamiento <- datos_modelo %>%
      filter(!test)
    
    validacion <- datos_modelo %>%
      filter(test)
    
    variables <- c(
      "region_origen",
      "room_type",
      "host_is_superhost",
      "accommodates",
      "bedrooms",
      "review_scores_rating",
      "number_of_reviews"
    )
    
    terminos <- c(
      "region_origen",
      "room_type",
      "host_is_superhost",
      "accommodates",
      "bedrooms",
      "review_scores_rating",
      "log1p(number_of_reviews)"
    )
    
    usar <- vapply(
      entrenamiento[variables],
      function(x) n_distinct(x) > 1,
      logical(1)
    )
    
    if (any(usar) && nrow(validacion) > 0) {
      
      formula_modelo <- as.formula(
        paste(
          "log(price) ~",
          paste(terminos[usar], collapse = " + ")
        )
      )
      
      modelo <- lm(
        formula_modelo,
        data = entrenamiento
      )
      
      # Corrección de retrans formación calculada solo con entrenamiento.
      correccion <- mean(exp(residuals(modelo)))
      
      validacion$estimado <- exp(
        predict(modelo, newdata = validacion)
      ) * correccion
      
      validacion <- validacion %>%
        filter(
          is.finite(estimado),
          estimado > 0
        ) %>%
        mutate(
          error = price - estimado,
          diferencia_pct = 100 * (price / estimado - 1)
        )
    }
  }
}

# ==============================================================================
# EJERCICIO 18: VALIDACIÓN DEL MODELO DE PRECIOS
# ==============================================================================

if (18 %in% EJERCICIOS) {
  
  if (!is.null(modelo) && nrow(validacion) > 0) {
    
    referencia <- entrenamiento %>%
      group_by(region_origen, room_type) %>%
      summarise(
        precio_base = median(price),
        .groups = "drop"
      )
    
    vv <- validacion %>%
      left_join(
        referencia,
        by = c("region_origen", "room_type")
      )
    
    denominador <- sum(
      (vv$price - mean(vv$price))^2
    )
    
    metricas <- tibble(
      MAE = mean(abs(vv$error)),
      RMSE = sqrt(mean(vv$error^2)),
      R2 = if (denominador > 0) {
        1 - sum(vv$error^2) / denominador
      } else {
        NA_real_
      },
      MAE_referencia = mean(abs(vv$price - vv$precio_base)),
      filas_entrenamiento = nrow(entrenamiento),
      filas_validacion = nrow(vv)
    )
    
    guardar_tabla(metricas, 18)
    
    write_csv(
      vv,
      file.path(carpeta_salida, "ej18_predicciones.csv")
    )
    
    limite <- unname(
      quantile(c(vv$price, vv$estimado), 0.99)
    )
    
    g <- ggplot(vv, aes(estimado, price)) +
      geom_point(
        colour = "#4F46E5",
        alpha = 0.2,
        size = 1.5
      ) +
      geom_abline(
        slope = 1,
        intercept = 0,
        colour = "#0D9488",
        linetype = 2
      ) +
      coord_fixed(
        xlim = c(0, limite),
        ylim = c(0, limite)
      ) +
      labs(
        title = "Validación del modelo de precios",
        subtitle = sprintf(
          "MAE %.2f € · RMSE %.2f € · R² %.3f · MAE referencia %.2f €",
          metricas$MAE,
          metricas$RMSE,
          metricas$R2,
          metricas$MAE_referencia
        ),
        x = "Precio estimado (€)",
        y = "Precio anunciado (€)",
        caption = paste(
          "Validación aleatoria reservada, no temporal ni por anfitrión.",
          "Zoom al percentil 99; métricas sobre todos los casos."
        )
      ) +
      tema_4k()
    
    exportar_4k(
      g,
      18,
      "Modelo de precios: validación",
      "Compara el MAE del modelo con el de la referencia. Menor MAE significa menor error medio."
    )
    
  } else {
    registrar(
      18,
      "Modelo de precios",
      nota = "Datos insuficientes para ajustar y validar el modelo."
    )
  }
}

# ==============================================================================
# EJERCICIO 19: SEGMENTACIÓN AUTOMÁTICA K-MEANS
# ==============================================================================

if (19 %in% EJERCICIOS) {
  
  d <- base_avanzada %>%
    filter(
      is.finite(accommodates),
      accommodates > 0,
      is.finite(bedrooms),
      bedrooms >= 0,
      review_scores_rating > 0,
      review_scores_rating <= 5,
      is.finite(number_of_reviews),
      number_of_reviews >= 0
    )
  
  variables <- d %>%
    transmute(
      Precio = log1p(price),
      Capacidad = log1p(accommodates),
      Dormitorios = log1p(bedrooms),
      Nota = review_scores_rating,
      Resenas = log1p(number_of_reviews)
    )
  
  variables <- variables[
    vapply(
      variables,
      function(x) is.finite(sd(x)) && sd(x) > 0,
      logical(1)
    )
  ]
  
  if (nrow(variables) >= 20 && ncol(variables) >= 2) {
    
    X <- scale(variables)
    
    max_k <- min(
      6,
      nrow(unique(as.data.frame(X))) - 1
    )
    
    if (max_k >= 2) {
      
      valores_k <- 2:max_k
      
      ajustes <- lapply(valores_k, function(k) {
        
        set.seed(1900 + k)
        
        kmeans(
          X,
          centers = k,
          nstart = 25,
          iter.max = 100
        )
      })
      
      comparacion <- tibble(
        K = valores_k,
        WSS = vapply(
          ajustes,
          function(m) m$tot.withinss,
          numeric(1)
        )
      ) %>%
        mutate(
          criterio =
            nrow(X) * log(
              pmax(WSS, .Machine$double.eps) / nrow(X)
            ) +
            K * ncol(X) * log(nrow(X))
        )
      
      elegido <- which.min(comparacion$criterio)
      
      d$segmento <- factor(
        ajustes[[elegido]]$cluster
      )
      
      perfiles <- d %>%
        group_by(segmento) %>%
        summarise(
          alojamientos = n(),
          precio_mediano = median(price),
          capacidad = mean(accommodates),
          dormitorios = mean(bedrooms),
          nota = mean(review_scores_rating),
          resenas = median(number_of_reviews),
          .groups = "drop"
        )
      
      guardar_tabla(perfiles, 19)
      
      write_csv(
        comparacion,
        file.path(carpeta_salida, "ej19_seleccion_K.csv")
      )
      
      write_csv(
        select(d, id, name, region_origen, segmento),
        file.path(carpeta_salida, "ej19_asignaciones.csv")
      )
      
      visual <- as_tibble(X) %>%
        mutate(segmento = d$segmento) %>%
        group_by(segmento) %>%
        summarise(
          across(everything(), mean),
          .groups = "drop"
        ) %>%
        pivot_longer(
          -segmento,
          names_to = "Variable",
          values_to = "Valor"
        )
      
      g <- ggplot(
        visual,
        aes(Variable, segmento, fill = Valor)
      ) +
        geom_tile(colour = "white", linewidth = 2) +
        geom_text(
          aes(label = sprintf("%+.2f", Valor)),
          size = 4.5,
          colour = "#17324D"
        ) +
        scale_fill_gradient2(
          low = "#99F6E4",
          mid = "#F8FAFC",
          high = "#C4B5FD",
          midpoint = 0,
          name = "Desviaciones\nestándar"
        ) +
        labs(
          title = "Segmentación multivariable de alojamientos",
          subtitle = paste(
            valores_k[elegido],
            "segmentos · selección heurística entre 2 y",
            max_k
          ),
          x = NULL,
          y = "Segmento",
          caption = paste(
            "Positivo: sobre la media; negativo: por debajo.",
            "Variables salvo nota transformadas con log1p.",
            "Los segmentos no son un ranking."
          )
        ) +
        tema_4k()
      
      exportar_4k(
        g,
        19,
        "Segmentación automática",
        "La selección de K es exploratoria. Consulta los perfiles en el CSV del ejercicio 19."
      )
      
    } else {
      registrar(
        19,
        "Segmentación automática",
        nota = "No hay suficientes perfiles diferentes."
      )
    }
    
  } else {
    registrar(
      19,
      "Segmentación automática",
      nota = "No hay suficientes datos o variables."
    )
  }
}

# ==============================================================================
# EJERCICIO 20: CASOS CON PRECIO INFERIOR AL ESTIMADO
# ==============================================================================

if (20 %in% EJERCICIOS) {
  
  if (!is.null(modelo) && nrow(validacion) > 0) {
    
    d <- validacion %>%
      filter(
        review_scores_rating >= 4.8,
        number_of_reviews >= 50,
        diferencia_pct <= -20
      ) %>%
      arrange(diferencia_pct) %>%
      slice_head(n = 10) %>%
      mutate(
        etiqueta = paste0(
          row_number(), ". ",
          str_trunc(coalesce(name, "Sin nombre"), 32),
          " · ",
          region_origen
        ),
        etiqueta = factor(
          etiqueta,
          levels = rev(etiqueta)
        )
      )
    
    guardar_tabla(d, 20)
    
    if (nrow(d) > 0) {
      
      g <- ggplot(d, aes(y = etiqueta)) +
        geom_segment(
          aes(
            x = price,
            xend = estimado,
            yend = etiqueta
          ),
          colour = "#CBD5E1",
          linewidth = 2
        ) +
        geom_point(
          aes(x = price, colour = "Anunciado"),
          size = 4
        ) +
        geom_point(
          aes(x = estimado, colour = "Estimado"),
          size = 4,
          shape = 18
        ) +
        geom_text(
          aes(
            x = estimado,
            label = paste0("  ", round(diferencia_pct), "%")
          ),
          hjust = 0,
          size = 4
        ) +
        scale_color_manual(
          values = c(
            Anunciado = "#0D9488",
            Estimado = "#8B5CF6"
          ),
          name = NULL
        ) +
        scale_x_continuous(
          expand = expansion(mult = c(0.03, 0.25))
        ) +
        labs(
          title = "Casos con precio inferior al estimado",
          subtitle = "Solo validación · nota ≥ 4,8 · reseñas ≥ 50 · diferencia ≥ 20 %",
          x = "Precio por noche (€)",
          y = NULL,
          caption = paste(
            "Selección para revisión, no prueba de infravaloración o rentabilidad.",
            "El modelo no incorpora barrio, fechas o estado del inmueble."
          )
        ) +
        tema_4k()
      
      exportar_4k(
        g,
        20,
        "Precio real frente a estimado"
      )
      
    } else {
      registrar(
        20,
        "Precio real frente a estimado",
        nota = paste(
          "El modelo se ha calculado, pero ningún caso de validación",
          "cumple todos los criterios."
        )
      )
    }
    
  } else {
    registrar(
      20,
      "Precio real frente a estimado",
      nota = "No se pudo construir el modelo necesario."
    )
  }
}

# ==============================================================================
# CREAR EL CUADRO DE MANDO DESPUÉS DE TODOS LOS EJERCICIOS
# ==============================================================================

crear_seccion <- function(numero) {
  
  resultado <- resultados[[as.character(numero)]]
  
  if (is.null(resultado)) return(NULL)
  
  T$section(
    class = if (numero >= 15) "tarjeta ancha" else "tarjeta",
    
    T$h2(
      paste0(numero, ". ", resultado$titulo)
    ),
    
    if (nzchar(resultado$nota)) {
      T$p(class = "nota", resultado$nota)
    },
    
    resultado$contenido
  )
}

crear_panel <- function(id, numeros, visible = FALSE) {
  
  T$div(
    id = id,
    class = "panel",
    style = if (visible) {
      "display:block;"
    } else {
      "display:none;"
    },
    
    T$div(
      class = "rejilla",
      htmltools::tagList(
        lapply(numeros, crear_seccion)
      )
    )
  )
}

grupos <- list(
  "Mercado" = c(1, 2, 5, 9, 12),
  "Anfitriones y barrios" = c(3, 7, 10, 14),
  "Selección" = c(4, 8, 11),
  "Reseñas y valoración" = c(6, 13),
  "Mapa" = 15,
  "Análisis avanzado" = 16:20
)

ids <- paste0("panel", seq_along(grupos))

paneles_con_datos <- which(
  vapply(
    grupos,
    function(numeros) {
      any(as.character(numeros) %in% names(resultados))
    },
    logical(1)
  )
)

primero <- if (length(paneles_con_datos) > 0) {
  paneles_con_datos[1]
} else {
  1
}

botones <- lapply(seq_along(grupos), function(i) {
  
  T$button(
    type = "button",
    class = if (i == primero) "activo" else "",
    onclick = sprintf(
      "abrir('%s',this)",
      ids[i]
    ),
    names(grupos)[i]
  )
})

css <- "
* { box-sizing: border-box; }

body {
  margin: 0;
  background: #EEF2F7;
  color: #17324D;
  font-family: Arial, sans-serif;
}

header {
  background: #17324D;
  color: white;
  padding: 25px 30px;
}

header h1 { margin: 0 0 8px; }

nav {
  display: flex;
  gap: 8px;
  flex-wrap: wrap;
  background: white;
  padding: 15px 24px;
}

button {
  border: 0;
  border-radius: 7px;
  background: #E8EEF5;
  color: #17324D;
  padding: 11px 15px;
  cursor: pointer;
  font-weight: bold;
}

button.activo {
  background: #0D9488;
  color: white;
}

main { padding: 20px 24px; }

.rejilla {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 18px;
  align-items: start;
}

.tarjeta {
  min-width: 0;
  background: white;
  border: 1px solid #DFE6EF;
  border-radius: 12px;
  padding: 20px;
  box-shadow: 0 3px 10px rgba(15,23,42,0.04);
}

.tarjeta h2 {
  font-size: 18px;
  margin: 0 0 12px;
}

.ancha { grid-column: 1 / -1; }

.nota {
  font-size: 12px;
  line-height: 1.6;
  color: #64748B;
}

.cards {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(170px, 1fr));
  gap: 12px;
}

.mini {
  background: #F5F7FC;
  border-radius: 10px;
  padding: 18px;
  border-top: 4px solid #14B8A6;
  overflow-wrap: anywhere;
}

.mini h3 {
  font-size: 14px;
  line-height: 1.5;
}

.cifra {
  font-size: 27px;
  font-weight: bold;
  color: #4F46E5;
}

.html-widget { max-width: 100%; }

details { margin: 15px 0; }

summary {
  cursor: pointer;
  color: #0D9488;
  font-weight: bold;
}

@media (max-width: 1150px) {
  .rejilla {
    grid-template-columns: minmax(0, 1fr);
  }
}
"

javascript <- "
function ajustar() {
  setTimeout(function() {

    window.dispatchEvent(new Event('resize'));

    if (window.Plotly) {
      document.querySelectorAll('.js-plotly-plot').forEach(function(e) {
        if (e.offsetParent !== null) {
          Plotly.Plots.resize(e);
        }
      });
    }

    if (window.jQuery && jQuery.fn.dataTable) {
      jQuery.fn.dataTable
        .tables({visible:true, api:true})
        .columns.adjust();
    }

  }, 180);
}

function abrir(id, boton) {

  document.querySelectorAll('.panel').forEach(function(e) {
    e.style.display = 'none';
  });

  document.getElementById(id).style.display = 'block';

  document.querySelectorAll('nav button').forEach(function(e) {
    e.classList.remove('activo');
  });

  boton.classList.add('activo');

  ajustar();
}

document.addEventListener('toggle', ajustar, true);
window.addEventListener('load', ajustar);
"

pagina <- T$html(
  lang = "es",
  
  T$head(
    T$meta(charset = "UTF-8"),
    T$meta(
      name = "viewport",
      content = "width=device-width,initial-scale=1"
    ),
    T$title("Airbnb Benelux — Cuadro de mando"),
    T$style(htmltools::HTML(css))
  ),
  
  T$body(
    
    T$header(
      T$h1("Airbnb Benelux"),
      T$p(
        paste(
          format(nrow(df), big.mark = "."),
          "registros · Exploración, mapa y análisis avanzado"
        )
      )
    ),
    
    T$nav(htmltools::tagList(botones)),
    
    T$main(
      htmltools::tagList(
        lapply(seq_along(grupos), function(i) {
          crear_panel(
            ids[i],
            grupos[[i]],
            visible = i == primero
          )
        })
      )
    ),
    
    T$script(htmltools::HTML(javascript))
  )
)

archivo_dashboard <- file.path(
  carpeta_salida,
  "00_dashboard_airbnb.html"
)

htmltools::save_html(
  pagina,
  file = archivo_dashboard,
  libdir = "dashboard_dependencias"
)

# ==============================================================================
# INICIAR EL SERVIDOR Y ABRIR EL DASHBOARD
# ==============================================================================

# Cerrar únicamente el servidor creado por este script en esta sesión.
if (exists(
  ".airbnb_servidor",
  envir = .GlobalEnv,
  inherits = FALSE
)) {
  
  anterior <- get(
    ".airbnb_servidor",
    envir = .GlobalEnv
  )
  
  if (
    is.list(anterior) &&
    is.function(anterior$stop_server)
  ) {
    try(anterior$stop_server(), silent = TRUE)
  }
}

# Al omitir port, servr elige un puerto disponible.
servidor <- servr::httd(
  dir = carpeta_salida,
  browser = FALSE,
  daemon = TRUE
)

assign(
  ".airbnb_servidor",
  servidor,
  envir = .GlobalEnv
)

direccion <- paste0(
  "http://127.0.0.1:",
  servidor$port,
  "/00_dashboard_airbnb.html"
)

utils::browseURL(direccion)

cat("\n====================================================\n")
cat("CUADRO DE MANDO GENERADO\n")
cat("Dirección:", direccion, "\n")
cat("Resultados:", carpeta_salida, "\n")
cat("Mantén RStudio abierto mientras consultas el dashboard.\n")
cat("====================================================\n")