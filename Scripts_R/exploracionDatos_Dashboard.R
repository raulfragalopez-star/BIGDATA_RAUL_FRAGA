# ==============================================================================
# AIRBNB BENELUX — DASHBOARD INTERACTIVO CON SHINY
# Guardar como app.R y pulsar Run App en RStudio
# ==============================================================================

# Instalar paquetes que falten
paquetes <- c(
  "shiny", "tidyverse", "readxl",
  "plotly", "DT", "leaflet"
)

faltantes <- paquetes[
  !vapply(
    paquetes,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(faltantes) > 0) {
  install.packages(faltantes)
}

library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(leaflet)

# ==============================================================================
# CONFIGURACIÓN
# ==============================================================================

# OPCIONAL: escribe una ruta fija para cargar siempre el mismo archivo.
# Usa barras /.
# Si está vacía, podrás subirlo desde el dashboard.
RUTA_FIJA <- ""

# Ejemplo:
# RUTA_FIJA <- "C:/Users/raulf/Desktop/.../archivo.xlsx"

# ==============================================================================
# FUNCIONES
# ==============================================================================

euros <- function(x) {
  
  if (length(x) == 0 || !is.finite(x)) {
    return("Sin datos")
  }
  
  paste0(
    formatC(
      x,
      format = "f",
      digits = 2,
      decimal.mark = ",",
      big.mark = "."
    ),
    " €"
  )
}

media_segura <- function(x) {
  
  x <- x[is.finite(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  mean(x)
}

escapar <- function(x) {
  
  as.character(
    htmltools::htmlEscape(
      ifelse(
        is.na(x),
        "Sin datos",
        as.character(x)
      )
    )
  )
}

leer_archivo <- function(
    ruta,
    nombre,
    separador = ",",
    decimal = ".",
    hoja = 1) {
  
  extension <- tolower(tools::file_ext(nombre))
  
  if (extension %in% c("xls", "xlsx")) {
    
    datos <- readxl::read_excel(
      ruta,
      sheet = hoja,
      col_types = "text"
    )
    
  } else if (extension == "csv") {
    
    datos <- readr::read_delim(
      ruta,
      delim = separador,
      col_types = cols(.default = col_character()),
      show_col_types = FALSE
    )
    
  } else {
    stop("Selecciona un archivo XLS, XLSX o CSV.")
  }
  
  necesarias <- c(
    "id", "name", "host_id", "host_name",
    "region_origen", "room_type",
    "host_is_superhost",
    "neighbourhood_cleansed",
    "price", "accommodates", "bedrooms",
    "review_scores_rating", "number_of_reviews"
  )
  
  faltan <- setdiff(necesarias, names(datos))
  
  if (length(faltan) > 0) {
    stop(paste(
      "Faltan columnas:",
      paste(faltan, collapse = ", "),
      ". Si todo aparece en una columna, revisa el separador CSV."
    ))
  }
  
  if (nrow(datos) == 0) {
    stop("El archivo o la hoja no contiene filas de datos.")
  }
  
  convertir <- function(x, columna) {
    
    texto <- trimws(as.character(x))
    
    texto[
      toupper(texto) %in% c(
        "", "NA", "N/A", "NULL", "NAN"
      )
    ] <- NA_character_
    
    texto <- gsub(
      "[$€£[:space:]]",
      "",
      texto
    )
    
    if (decimal == ",") {
      
      texto <- gsub(
        ".", "",
        texto,
        fixed = TRUE
      )
      
      texto <- gsub(
        ",", ".",
        texto,
        fixed = TRUE
      )
      
    } else {
      
      texto <- gsub(
        ",", "",
        texto,
        fixed = TRUE
      )
    }
    
    valor <- suppressWarnings(as.numeric(texto))
    
    errores <- !is.na(texto) & !is.finite(valor)
    
    if (any(errores)) {
      
      stop(paste(
        "Revisa el formato numérico de",
        columna,
        ". Ejemplos:",
        paste(
          head(unique(texto[errores]), 5),
          collapse = ", "
        )
      ))
    }
    
    valor
  }
  
  numericas <- intersect(
    c(
      "price", "bedrooms", "beds",
      "accommodates", "review_scores_rating",
      "number_of_reviews"
    ),
    names(datos)
  )
  
  for (columna in numericas) {
    
    datos[[columna]] <- convertir(
      datos[[columna]],
      columna
    )
  }
  
  datos <- datos %>%
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
  
  datos
}

reconstruir_coordenada <- function(x, posiciones) {
  
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
  
  valor[enteros] <- suppressWarnings(
    as.numeric(
      paste0(
        substr(valores, 1, posiciones),
        ".",
        substring(valores, posiciones + 1)
      )
    )
  )
  
  valor
}

tema <- function() {
  
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(
        face = "bold",
        color = "#17324D"
      ),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.title = element_text(color = "#475569"),
      strip.text = element_text(
        face = "bold",
        color = "#17324D"
      )
    )
}

crear_widget <- function(grafico) {
  
  plotly::ggplotly(grafico) %>%
    plotly::layout(
      paper_bgcolor = "white",
      plot_bgcolor = "white",
      margin = list(
        l = 80,
        r = 40,
        b = 90,
        t = 60
      )
    ) %>%
    plotly::config(
      displaylogo = FALSE,
      responsive = TRUE
    )
}

ranking <- function(
    datos,
    variable,
    titulo,
    eje,
    color = "#4F46E5") {
  
  datos <- datos %>%
    mutate(
      etiqueta = paste0(
        row_number(), ". ",
        str_trunc(coalesce(name, "Sin nombre"), 35)
      ),
      etiqueta = factor(
        etiqueta,
        levels = rev(etiqueta)
      )
    )
  
  ggplot(
    datos,
    aes(
      x = .data[[variable]],
      y = etiqueta
    )
  ) +
    geom_col(
      fill = color,
      width = 0.65
    ) +
    labs(
      title = titulo,
      x = eje,
      y = NULL
    ) +
    tema()
}

ejercicios <- c(
  "1 · Alojamientos más caros" = "1",
  "2 · Mercado por ciudad" = "2",
  "3 · Superhost" = "3",
  "4 · Selección en Ámsterdam" = "4",
  "5 · Tipologías por ciudad" = "5",
  "6 · Ranking de reseñas" = "6",
  "7 · Barrios" = "7",
  "8 · Alojamientos para grupos" = "8",
  "9 · Distribución de precios" = "9",
  "10 · Mayores anfitriones" = "10",
  "11 · Precio frente a referencia" = "11",
  "12 · Composición porcentual" = "12",
  "13 · Precio, nota y reseñas en 3D" = "13",
  "14 · Diferencia Superhost" = "14"
)

# ==============================================================================
# INTERFAZ
# ==============================================================================

ui <- fluidPage(
  
  tags$head(
    
    tags$style(HTML("
      body {
        background: #EEF2F7;
        font-family: Arial, sans-serif;
        color: #17324D;
      }

      .cabecera {
        background: #17324D;
        color: white;
        padding: 24px;
        border-radius: 10px;
        margin: 15px 0 20px;
      }

      .cabecera h2 {
        margin: 0 0 8px;
        font-weight: bold;
      }

      .well {
        background: white;
        border: 1px solid #DFE6EF;
        border-radius: 12px;
      }

      .bloque {
        background: white;
        padding: 20px;
        border-radius: 12px;
        border: 1px solid #DFE6EF;
        margin-bottom: 18px;
      }

      .kpis {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 12px;
        margin-bottom: 20px;
      }

      .kpi {
        background: white;
        border-radius: 12px;
        border-top: 4px solid #14B8A6;
        padding: 18px;
      }

      .kpi span {
        display: block;
        color: #64748B;
        font-size: 12px;
        margin-bottom: 8px;
      }

      .kpi strong {
        color: #4F46E5;
        font-size: 25px;
      }

      .btn-primary {
        background: #4F46E5;
        border-color: #4F46E5;
      }

      .nota {
        color: #64748B;
        font-size: 12px;
        line-height: 1.6;
      }

      .nav-tabs {
        margin-bottom: 18px;
      }

      @media(max-width: 900px) {
        .kpis {
          grid-template-columns: repeat(2, minmax(0,1fr));
        }
      }
    "))
  ),
  
  div(
    class = "cabecera",
    h2("Airbnb Benelux"),
    p("Cuadro de mando con filtros y análisis avanzado")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      width = 3,
      
      h4("Fuente de datos"),
      
      fileInput(
        "archivo",
        "Seleccionar Excel o CSV",
        accept = c(".xlsx", ".xls", ".csv")
      ),
      
      selectInput(
        "separador",
        "Separador CSV",
        choices = c(
          "Coma" = ",",
          "Punto y coma" = ";",
          "Tabulador" = "\t"
        )
      ),
      
      selectInput(
        "decimal",
        "Separador decimal",
        choices = c(
          "Punto: 100.50" = ".",
          "Coma: 100,50" = ","
        )
      ),
      
      numericInput(
        "hoja",
        "Número de hoja del Excel",
        value = 1,
        min = 1,
        step = 1
      ),
      
      actionButton(
        "cargar",
        "Cargar datos",
        class = "btn-primary"
      ),
      
      hr(),
      
      h4("Filtros globales"),
      
      selectizeInput(
        "ciudad",
        "Ciudad",
        choices = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "Selecciona ciudades"
        )
      ),
      
      selectizeInput(
        "tipo",
        "Tipo de alojamiento",
        choices = NULL,
        multiple = TRUE
      ),
      
      sliderInput(
        "precio",
        "Precio por noche (€)",
        min = 0,
        max = 1000,
        value = c(0, 1000),
        step = 1
      ),
      
      numericInput(
        "capacidad",
        "Capacidad mínima",
        value = 0,
        min = 0
      ),
      
      numericInput(
        "dormitorios",
        "Dormitorios mínimos",
        value = 0,
        min = 0
      ),
      
      sliderInput(
        "nota",
        "Valoración mínima",
        min = 0,
        max = 5,
        value = 0,
        step = 0.1
      ),
      
      numericInput(
        "resenas",
        "Número mínimo de reseñas",
        value = 0,
        min = 0
      ),
      
      selectInput(
        "superhost",
        "Superhost",
        choices = c(
          "Todos" = "todos",
          "Sí" = "t",
          "No" = "f",
          "Sin información" = "sin"
        )
      ),
      
      actionButton(
        "restablecer",
        "Restablecer filtros"
      ),
      
      br(),
      br(),
      
      downloadButton(
        "descargar",
        "Descargar datos filtrados"
      ),
      
      hr(),
      
      textOutput("estado")
    ),
    
    mainPanel(
      
      width = 9,
      
      uiOutput("indicadores"),
      
      tabsetPanel(
        
        tabPanel(
          "Exploración",
          
          div(
            class = "bloque",
            
            selectInput(
              "ejercicio",
              "Ejercicio",
              choices = ejercicios,
              selected = "2"
            ),
            
            conditionalPanel(
              condition = "input.ejercicio == '6'",
              
              selectInput(
                "ranking_resenas",
                "Grupo",
                choices = c(
                  "Más reseñas" = "mas",
                  "Menos entre los que superan 250" = "menos"
                )
              )
            ),
            
            plotlyOutput(
              "grafico",
              height = "580px"
            ),
            
            uiOutput("nota_ejercicio")
          ),
          
          div(
            class = "bloque",
            h4("Tabla del ejercicio"),
            DTOutput("tabla_ejercicio")
          )
        ),
        
        tabPanel(
          "Datos filtrados",
          
          div(
            class = "bloque",
            DTOutput("tabla_datos")
          )
        ),
        
        tabPanel(
          "Mapa",
          
          div(
            class = "bloque",
            
            checkboxInput(
              "reconstruir",
              "Reconstruir coordenadas sin separador decimal",
              value = TRUE
            ),
            
            p(
              class = "nota",
              paste(
                "La reconstrucción es provisional para los valores",
                "de Benelux mostrados anteriormente.",
                "Desactívala si utilizas coordenadas originales correctas."
              )
            ),
            
            leafletOutput(
              "mapa",
              height = "650px"
            ),
            
            textOutput("estado_mapa")
          )
        ),
        
        tabPanel(
          "Análisis avanzado",
          
          div(
            class = "bloque",
            
            p(
              "Aplica los filtros y pulsa el botón para recalcular."
            ),
            
            selectInput(
              "avanzado",
              "Análisis",
              choices = c(
                "16 · Bootstrap de precios" = "16",
                "17 · Concentración de anfitriones" = "17",
                "18 · Modelo de precios" = "18",
                "19 · Segmentación de alojamientos" = "19",
                "20 · Precio real frente a estimado" = "20"
              )
            ),
            
            actionButton(
              "calcular",
              "Calcular análisis",
              class = "btn-primary"
            ),
            
            br(),
            br(),
            
            uiOutput("estado_avanzado"),
            
            plotOutput(
              "grafico_avanzado",
              height = "600px"
            ),
            
            downloadButton(
              "descargar_4k",
              "Descargar gráfico 4K"
            ),
            
            br(),
            br(),
            
            DTOutput("tabla_avanzada")
          )
        )
      )
    )
  )
)

# ==============================================================================
# SERVIDOR
# ==============================================================================

server <- function(input, output, session) {
  
  base <- reactiveVal(NULL)
  error_carga <- reactiveVal("")
  version_datos <- reactiveVal(0)
  
  # ---------------------------------------------------------------------------
  # CARGA Y RESTABLECIMIENTO
  # ---------------------------------------------------------------------------
  
  configurar_filtros <- function(d) {
    
    precios <- d$price[
      is.finite(d$price) & d$price > 0
    ]
    
    maximo <- if (length(precios) > 0) {
      max(1, ceiling(max(precios)))
    } else {
      1000
    }
    
    ciudades <- sort(unique(na.omit(d$region_origen)))
    tipos <- sort(unique(na.omit(d$room_type)))
    
    updateSelectizeInput(
      session,
      "ciudad",
      choices = ciudades,
      selected = ciudades,
      server = TRUE
    )
    
    updateSelectizeInput(
      session,
      "tipo",
      choices = tipos,
      selected = tipos,
      server = TRUE
    )
    
    updateSliderInput(
      session,
      "precio",
      min = 0,
      max = maximo,
      value = c(0, maximo)
    )
    
    updateNumericInput(session, "capacidad", value = 0)
    updateNumericInput(session, "dormitorios", value = 0)
    updateSliderInput(session, "nota", value = 0)
    updateNumericInput(session, "resenas", value = 0)
    updateSelectInput(session, "superhost", selected = "todos")
  }
  
  cargar_datos <- function(ruta, nombre) {
    
    tryCatch({
      
      d <- leer_archivo(
        ruta,
        nombre,
        separador = input$separador,
        decimal = input$decimal,
        hoja = input$hoja
      )
      
      base(d)
      error_carga("")
      version_datos(version_datos() + 1)
      configurar_filtros(d)
      
      showNotification(
        paste("Datos cargados:", nrow(d), "filas."),
        type = "message"
      )
      
    }, error = function(e) {
      
      error_carga(conditionMessage(e))
      
      showNotification(
        conditionMessage(e),
        type = "error",
        duration = NULL
      )
    })
  }
  
  observeEvent(input$cargar, {
    
    if (!is.null(input$archivo)) {
      
      cargar_datos(
        input$archivo$datapath,
        input$archivo$name
      )
      
    } else if (
      nzchar(RUTA_FIJA) &&
      file.exists(RUTA_FIJA)
    ) {
      
      cargar_datos(
        RUTA_FIJA,
        basename(RUTA_FIJA)
      )
      
    } else {
      
      showNotification(
        "Selecciona primero un archivo.",
        type = "warning"
      )
    }
  })
  
  observeEvent(TRUE, {
    
    if (nzchar(RUTA_FIJA) && file.exists(RUTA_FIJA)) {
      
      cargar_datos(
        RUTA_FIJA,
        basename(RUTA_FIJA)
      )
    }
    
  }, once = TRUE)
  
  observeEvent(input$restablecer, {
    
    req(base())
    configurar_filtros(base())
  })
  
  # ---------------------------------------------------------------------------
  # FILTRADO REACTIVO
  # ---------------------------------------------------------------------------
  
  filtrados <- reactive({
    
    req(base())
    
    d <- base()
    
    d <- d %>%
      filter(
        region_origen %in% input$ciudad,
        room_type %in% input$tipo,
        is.finite(price),
        price > 0,
        price >= input$precio[1],
        price <= input$precio[2]
      )
    
    if (input$capacidad > 0) {
      d <- d %>%
        filter(accommodates >= input$capacidad)
    }
    
    if (input$dormitorios > 0) {
      d <- d %>%
        filter(bedrooms >= input$dormitorios)
    }
    
    if (input$nota > 0) {
      d <- d %>%
        filter(
          review_scores_rating >= input$nota,
          review_scores_rating <= 5
        )
    }
    
    if (input$resenas > 0) {
      d <- d %>%
        filter(number_of_reviews >= input$resenas)
    }
    
    if (input$superhost %in% c("t", "f")) {
      d <- d %>%
        filter(host_is_superhost == input$superhost)
    }
    
    if (input$superhost == "sin") {
      d <- d %>%
        filter(is.na(host_is_superhost))
    }
    
    d
  })
  
  output$estado <- renderText({
    
    if (nzchar(error_carga())) {
      return(error_carga())
    }
    
    if (is.null(base())) {
      return("Carga un archivo para comenzar.")
    }
    
    paste(
      nrow(filtrados()),
      "filas seleccionadas de",
      nrow(base()),
      "filas cargadas."
    )
  })
  
  # ---------------------------------------------------------------------------
  # INDICADORES
  # ---------------------------------------------------------------------------
  
  output$indicadores <- renderUI({
    
    req(base())
    
    d <- filtrados()
    
    nota_valida <- d$review_scores_rating[
      is.finite(d$review_scores_rating) &
        d$review_scores_rating > 0 &
        d$review_scores_rating <= 5
    ]
    
    mediana <- if (nrow(d) > 0) {
      median(d$price)
    } else {
      NA_real_
    }
    
    nota_media <- media_segura(nota_valida)
    
    kpi <- function(titulo, valor) {
      div(
        class = "kpi",
        tags$span(titulo),
        tags$strong(valor)
      )
    }
    
    div(
      class = "kpis",
      kpi(
        "Alojamientos / filas seleccionadas",
        format(nrow(d), big.mark = ".")
      ),
      kpi("Precio medio", euros(media_segura(d$price))),
      kpi("Precio mediano", euros(mediana)),
      kpi(
        "Nota media válida",
        if (is.finite(nota_media)) {
          sprintf("%.2f / 5", nota_media)
        } else {
          "Sin datos"
        }
      )
    )
  })
  
  # ---------------------------------------------------------------------------
  # TABLA GENERAL Y DESCARGA
  # ---------------------------------------------------------------------------
  
  output$tabla_datos <- renderDT({
    
    req(base())
    
    datatable(
      filtrados(),
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 15,
        scrollX = TRUE
      )
    )
    
  }, server = TRUE)
  
  output$descargar <- downloadHandler(
    
    filename = function() {
      paste0(
        "airbnb_filtrado_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      write_csv(filtrados(), file, na = "")
    }
  )
  
  # ---------------------------------------------------------------------------
  # RESULTADOS DE LOS EJERCICIOS 1–14
  # ---------------------------------------------------------------------------
  
  resultado <- reactive({
    
    d <- filtrados()
    
    validate(
      need(
        nrow(d) > 0,
        "No hay alojamientos con estos filtros."
      )
    )
    
    e <- as.integer(input$ejercicio)
    
    nota <- paste(
      "Los cálculos utilizan la selección actual.",
      "Los precios ausentes o no positivos quedan excluidos."
    )
    
    if (e == 1) {
      
      r <- d %>%
        arrange(desc(price)) %>%
        slice_head(n = 5)
      
      g <- ranking(
        r, "price",
        "Los cinco alojamientos más caros",
        "Precio por noche (€)"
      )
      
    } else if (e == 2) {
      
      r <- d %>%
        group_by(region_origen) %>%
        summarise(
          Alojamientos = n(),
          Precio_medio = mean(price),
          .groups = "drop"
        )
      
      g <- ggplot(
        r,
        aes(
          x = reorder(region_origen, Precio_medio),
          y = Precio_medio
        )
      ) +
        geom_col(
          fill = "#4F46E5",
          width = 0.65
        ) +
        geom_text(
          aes(label = paste0("n = ", Alojamientos)),
          hjust = -0.1,
          size = 4
        ) +
        coord_flip() +
        scale_y_continuous(
          expand = expansion(mult = c(0, 0.25))
        ) +
        labs(
          title = "Precio medio y volumen por ciudad",
          x = NULL,
          y = "Precio medio (€)"
        ) +
        tema()
      
    } else if (e == 3) {
      
      r <- d %>%
        filter(
          review_scores_rating > 0,
          review_scores_rating <= 5,
          host_is_superhost %in% c("t", "f")
        ) %>%
        mutate(
          Perfil = if_else(
            host_is_superhost == "t",
            "Superhost",
            "Otros anfitriones"
          )
        ) %>%
        group_by(Perfil) %>%
        summarise(
          Precio_medio = mean(price),
          Nota_media = mean(review_scores_rating),
          Alojamientos = n(),
          .groups = "drop"
        )
      
      g <- ggplot(
        r,
        aes(Perfil, Precio_medio, fill = Perfil)
      ) +
        geom_col(width = 0.55) +
        scale_fill_manual(
          values = c("#A78BFA", "#14B8A6")
        ) +
        labs(
          title = "Precio medio según perfil",
          x = NULL,
          y = "Precio medio (€)"
        ) +
        tema()
      
      nota <- paste(
        "Solo alojamientos con nota válida.",
        "Si filtras un único perfil Superhost,",
        "la comparación tendrá una sola categoría.",
        "No demuestra causalidad."
      )
      
    } else if (e == 4) {
      
      r <- d %>%
        filter(
          region_origen == "Amsterdam (Holanda)",
          room_type == "Entire home/apt",
          price > 80,
          price < 200,
          accommodates >= 4,
          bedrooms >= 3,
          review_scores_rating >= 4
        ) %>%
        arrange(price)
      
      g <- ranking(
        slice_head(r, n = 10),
        "price",
        "Selección en Ámsterdam",
        "Precio por noche (€)",
        "#14B8A6"
      )
      
      nota <- paste(
        "Vivienda completa en Ámsterdam:",
        "80–200 €, capacidad mínima 4,",
        "al menos 3 dormitorios y nota mínima 4.",
        "Estos criterios se añaden a tus filtros."
      )
      
    } else if (e == 5) {
      
      r <- d %>%
        count(
          region_origen,
          room_type,
          name = "Alojamientos"
        )
      
      g <- ggplot(
        r,
        aes(
          region_origen,
          Alojamientos,
          fill = room_type
        )
      ) +
        geom_col(
          position = "dodge",
          width = 0.75
        ) +
        coord_flip() +
        scale_fill_manual(
          values = c(
            "#4F46E5", "#14B8A6",
            "#A78BFA", "#F472B6"
          )
        ) +
        labs(
          title = "Tipologías por ciudad",
          x = NULL,
          y = "Número de alojamientos"
        ) +
        tema()
      
    } else if (e == 6) {
      
      if (input$ranking_resenas == "mas") {
        
        r <- d %>%
          filter(number_of_reviews > 0) %>%
          arrange(desc(number_of_reviews)) %>%
          slice_head(n = 10)
        
      } else {
        
        r <- d %>%
          filter(number_of_reviews > 250) %>%
          arrange(number_of_reviews) %>%
          slice_head(n = 10)
      }
      
      rr <- r %>%
        mutate(
          etiqueta = paste0(
            row_number(), ". ",
            str_trunc(coalesce(name, "Sin nombre"), 35)
          ),
          etiqueta = factor(
            etiqueta,
            levels = rev(etiqueta)
          )
        )
      
      g <- ggplot(
        rr,
        aes(number_of_reviews, etiqueta)
      ) +
        geom_segment(
          aes(
            x = 0,
            xend = number_of_reviews,
            yend = etiqueta
          ),
          color = "#DFE7F1",
          linewidth = 2
        ) +
        geom_point(
          color = "#4F46E5",
          size = 4
        ) +
        labs(
          title = "Ranking de reseñas",
          x = "Número de reseñas",
          y = NULL
        ) +
        tema()
      
    } else if (e == 7) {
      
      r <- d %>%
        filter(!is.na(neighbourhood_cleansed)) %>%
        group_by(
          region_origen,
          neighbourhood_cleansed
        ) %>%
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
        mutate(
          zona = paste(
            region_origen,
            neighbourhood_cleansed,
            sep = " · "
          )
        )
      
      g <- ggplot(
        r,
        aes(
          Alojamientos,
          reorder(zona, Alojamientos)
        )
      ) +
        geom_col(
          fill = "#14B8A6",
          width = 0.65
        ) +
        labs(
          title = "Barrios con más alojamientos",
          x = "Alojamientos",
          y = NULL
        ) +
        tema()
      
    } else if (e == 8) {
      
      r <- d %>%
        filter(
          room_type == "Entire home/apt",
          accommodates >= 6,
          price < 150
        ) %>%
        mutate(
          precio_persona = price / accommodates
        ) %>%
        arrange(precio_persona) %>%
        slice_head(n = 10)
      
      g <- ranking(
        r,
        "precio_persona",
        "Opciones para grupos",
        "Precio por persona y noche (€)",
        "#14B8A6"
      )
      
      nota <- paste(
        "Vivienda completa para al menos seis personas.",
        "Precio por persona a ocupación completa;",
        "no incluye cargos adicionales."
      )
      
    } else if (e == 9) {
      
      r <- d %>%
        group_by(region_origen) %>%
        summarise(
          Minimo = min(price),
          Mediana = median(price),
          Media = mean(price),
          Maximo = max(price),
          .groups = "drop"
        )
      
      g <- ggplot(
        d,
        aes(price, region_origen)
      ) +
        geom_boxplot(
          fill = "#C4B5FD",
          color = "#4F46E5",
          outlier.shape = NA
        ) +
        labs(
          title = "Distribución de precios",
          x = "Precio por noche (€)",
          y = NULL
        ) +
        tema()
      
      nota <- paste(
        "Los puntos extremos se ocultan;",
        "no se eliminan de las estadísticas."
      )
      
    } else if (e == 10) {
      
      r <- d %>%
        filter(
          !is.na(host_id),
          !is.na(id)
        ) %>%
        distinct(host_id, id, .keep_all = TRUE) %>%
        group_by(host_id) %>%
        summarise(
          name = first(
            coalesce(host_name, "Sin nombre")
          ),
          Alojamientos = n(),
          .groups = "drop"
        ) %>%
        arrange(desc(Alojamientos)) %>%
        slice_head(n = 10)
      
      g <- ranking(
        r,
        "Alojamientos",
        "Mayores anfitriones de la selección",
        "Alojamientos distintos"
      )
      
    } else if (e == 11) {
      
      r <- d %>%
        group_by(region_origen, room_type) %>%
        mutate(
          referencia = median(price),
          diferencia_pct =
            100 * (1 - price / referencia)
        ) %>%
        ungroup() %>%
        filter(
          diferencia_pct >= 30,
          review_scores_rating >= 4.8,
          review_scores_rating <= 5,
          number_of_reviews >= 50
        ) %>%
        arrange(desc(diferencia_pct)) %>%
        slice_head(n = 8)
      
      rr <- r %>%
        mutate(
          etiqueta = paste0(
            row_number(), ". ",
            str_trunc(coalesce(name, "Sin nombre"), 30)
          ),
          etiqueta = factor(
            etiqueta,
            levels = rev(etiqueta)
          )
        )
      
      g <- ggplot(
        rr,
        aes(y = etiqueta)
      ) +
        geom_segment(
          aes(
            x = price,
            xend = referencia,
            yend = etiqueta
          ),
          color = "#CBD5E1",
          linewidth = 2
        ) +
        geom_point(
          aes(x = price, color = "Precio real"),
          size = 4
        ) +
        geom_point(
          aes(x = referencia, color = "Mediana"),
          size = 4,
          shape = 18
        ) +
        scale_color_manual(
          values = c(
            "Precio real" = "#14B8A6",
            "Mediana" = "#8B5CF6"
          )
        ) +
        labs(
          title = "Precio real frente a referencia",
          x = "Precio por noche (€)",
          y = NULL
        ) +
        tema()
      
      nota <- paste(
        "La mediana se recalcula dentro de la selección,",
        "por ciudad y tipología.",
        "No es una rebaja anunciada."
      )
      
    } else if (e == 12) {
      
      r <- d %>%
        count(region_origen, room_type) %>%
        group_by(region_origen) %>%
        mutate(
          Porcentaje = 100 * n / sum(n)
        ) %>%
        ungroup()
      
      g <- ggplot(
        r,
        aes(
          region_origen,
          Porcentaje,
          fill = room_type
        )
      ) +
        geom_col() +
        coord_flip() +
        scale_fill_manual(
          values = c(
            "#4F46E5", "#14B8A6",
            "#A78BFA", "#F472B6"
          )
        ) +
        labs(
          title = "Composición porcentual",
          x = NULL,
          y = "Porcentaje"
        ) +
        tema()
      
    } else if (e == 13) {
      
      r <- d %>%
        filter(
          review_scores_rating > 0,
          review_scores_rating <= 5,
          number_of_reviews >= 20
        )
      
      total <- nrow(r)
      
      set.seed(123)
      
      if (nrow(r) > 2000) {
        r <- r %>%
          group_by(region_origen) %>%
          slice_sample(prop = 2000 / total) %>%
          ungroup()
      }
      
      nota <- paste(
        nrow(r), "de", total,
        "alojamientos válidos representados.",
        "Arrastra para girar. Precio en escala logarítmica."
      )
      
      validate(
        need(nrow(r) > 0, "No hay casos válidos para el 3D.")
      )
      
      p <- plotly::plot_ly(
        r,
        x = ~price,
        y = ~review_scores_rating,
        z = ~number_of_reviews,
        color = ~region_origen,
        colors = c(
          "#4F46E5", "#14B8A6",
          "#A78BFA", "#F472B6", "#38BDF8"
        ),
        type = "scatter3d",
        mode = "markers",
        marker = list(
          size = 3,
          opacity = 0.7
        ),
        text = ~paste0(
          escapar(name),
          "<br>",
          escapar(region_origen)
        ),
        hovertemplate = paste0(
          "%{text}<br>Precio: %{x:.2f} €",
          "<br>Nota: %{y}",
          "<br>Reseñas: %{z}<extra></extra>"
        )
      ) %>%
        plotly::layout(
          scene = list(
            xaxis = list(
              title = "Precio (€)",
              type = "log"
            ),
            yaxis = list(title = "Nota"),
            zaxis = list(title = "Reseñas"),
            aspectmode = "cube"
          ),
          margin = list(
            l = 0, r = 0,
            t = 20, b = 70
          ),
          legend = list(orientation = "h")
        ) %>%
        plotly::config(
          displaylogo = FALSE,
          responsive = TRUE
        )
      
      return(list(
        tabla = r,
        widget = p,
        nota = nota
      ))
      
    } else if (e == 14) {
      
      r <- d %>%
        filter(
          host_is_superhost %in% c("f", "t")
        ) %>%
        group_by(
          region_origen,
          room_type,
          host_is_superhost
        ) %>%
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
        if (!columna %in% names(r)) {
          r[[columna]] <- rep(NA_real_, nrow(r))
        }
      }
      
      r <- r %>%
        mutate(
          Diferencia =
            100 * (precio_t / precio_f - 1)
        )
      
      validate(
        need(
          any(is.finite(r$Diferencia)),
          "Selecciona ambos perfiles Superhost para compararlos."
        )
      )
      
      g <- ggplot(
        r,
        aes(
          room_type,
          region_origen,
          fill = Diferencia
        )
      ) +
        geom_tile(
          color = "white",
          linewidth = 1
        ) +
        geom_text(
          aes(label = ifelse(
            is.finite(Diferencia),
            sprintf("%+.1f%%", Diferencia),
            "—"
          )),
          size = 4
        ) +
        scale_fill_gradient2(
          low = "#99F6E4",
          mid = "#F8FAFC",
          high = "#C4B5FD",
          midpoint = 0,
          na.value = "#F1F5F9"
        ) +
        labs(
          title = "Diferencia de precio Superhost",
          x = NULL,
          y = NULL
        ) +
        tema()
      
      nota <- paste(
        "Diferencia respecto a otros anfitriones",
        "de la misma ciudad y tipología.",
        "No ajusta capacidad ni barrio."
      )
    }
    
    validate(
      need(
        nrow(r) > 0,
        "No hay resultados para este ejercicio con los filtros actuales."
      )
    )
    
    list(
      tabla = r,
      widget = crear_widget(g),
      nota = nota
    )
  })
  
  output$grafico <- renderPlotly({
    resultado()$widget
  })
  
  output$tabla_ejercicio <- renderDT({
    
    datatable(
      resultado()$tabla,
      rownames = FALSE,
      options = list(
        pageLength = 8,
        scrollX = TRUE
      )
    )
    
  }, server = TRUE)
  
  output$nota_ejercicio <- renderUI({
    p(class = "nota", resultado()$nota)
  })
  
  # ---------------------------------------------------------------------------
  # MAPA REACTIVO
  # ---------------------------------------------------------------------------
  
  datos_mapa <- reactive({
    
    d <- filtrados()
    
    validate(
      need(
        all(c("latitude", "longitude") %in% names(d)),
        "El archivo no tiene latitude y longitude."
      )
    )
    
    if (isTRUE(input$reconstruir)) {
      
      d$lat <- reconstruir_coordenada(
        d$latitude, 2
      )
      
      d$lon <- reconstruir_coordenada(
        d$longitude, 1
      )
      
    } else {
      
      d$lat <- suppressWarnings(
        as.numeric(
          gsub(",", ".", d$latitude, fixed = TRUE)
        )
      )
      
      d$lon <- suppressWarnings(
        as.numeric(
          gsub(",", ".", d$longitude, fixed = TRUE)
        )
      )
    }
    
    d %>%
      filter(
        is.finite(lat),
        is.finite(lon),
        between(lat, 49, 54),
        between(lon, 2, 8)
      )
  })
  
  output$mapa <- renderLeaflet({
    
    d <- datos_mapa()
    
    validate(
      need(
        nrow(d) > 0,
        "No hay coordenadas válidas con esta selección."
      )
    )
    
    d$ficha <- paste0(
      "<b>", escapar(d$name), "</b>",
      "<br>Ciudad: ", escapar(d$region_origen),
      "<br>Tipo: ", escapar(d$room_type),
      "<br>Precio: ",
      vapply(d$price, euros, character(1)),
      "<br>Capacidad: ", d$accommodates,
      "<br>Dormitorios: ", d$bedrooms,
      "<br>Nota: ", d$review_scores_rating,
      "<br>Reseñas: ", d$number_of_reviews
    )
    
    mapa <- leaflet(d) %>%
      addTiles(
        urlTemplate =
          "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = "&copy; OpenStreetMap contributors"
      ) %>%
      addMarkers(
        lng = ~lon,
        lat = ~lat,
        popup = ~ficha,
        clusterOptions = markerClusterOptions(
          chunkedLoading = TRUE
        )
      )
    
    if (
      diff(range(d$lon)) == 0 &&
      diff(range(d$lat)) == 0
    ) {
      
      mapa <- mapa %>%
        setView(
          lng = d$lon[1],
          lat = d$lat[1],
          zoom = 13
        )
      
    } else {
      
      mapa <- mapa %>%
        fitBounds(
          min(d$lon), min(d$lat),
          max(d$lon), max(d$lat)
        )
    }
    
    mapa
  })
  
  output$estado_mapa <- renderText({
    
    paste(
      nrow(datos_mapa()),
      "alojamientos representados de",
      nrow(filtrados()),
      "filas filtradas."
    )
  })
  
  # ---------------------------------------------------------------------------
  # ANÁLISIS AVANZADO: SE EJECUTA SOLO AL PULSAR EL BOTÓN
  # ---------------------------------------------------------------------------
  
  analisis <- eventReactive(input$calcular, {
    
    req(base())
    
    withProgress(
      message = "Calculando análisis avanzado",
      value = 0,
      {
        
        d <- filtrados() %>%
          filter(!is.na(id)) %>%
          distinct(id, .keep_all = TRUE)
        
        validate(
          need(
            nrow(d) >= 10,
            "Selecciona al menos diez alojamientos con ID."
          )
        )
        
        numero <- as.integer(input$avanzado)
        firma <- firma_filtros()
        
        incProgress(0.15)
        
        # EJERCICIO 16 --------------------------------------------------------
        
        if (numero == 16) {
          
          grupos <- d %>%
            count(region_origen, room_type) %>%
            filter(n >= 30)
          
          dd <- d %>%
            semi_join(
              grupos,
              by = c("region_origen", "room_type")
            )
          
          validate(
            need(
              nrow(dd) > 0,
              "Necesitas al menos 30 alojamientos por ciudad y tipología."
            )
          )
          
          set.seed(1601)
          
          r <- dd %>%
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
                inferior = unname(
                  quantile(medianas, 0.025)
                ),
                superior = unname(
                  quantile(medianas, 0.975)
                )
              )
            }) %>%
            ungroup()
          
          g <- ggplot(
            r,
            aes(mediana, region_origen)
          ) +
            geom_segment(
              aes(
                x = inferior,
                xend = superior,
                yend = region_origen
              ),
              color = "#A78BFA",
              linewidth = 2
            ) +
            geom_point(
              color = "#4F46E5",
              size = 3
            ) +
            facet_wrap(
              ~room_type,
              scales = "free_x",
              ncol = 2
            ) +
            labs(
              title = "16. Precio mediano e incertidumbre",
              subtitle = "Intervalos bootstrap del 95 % · 1.000 remuestreos",
              x = "Precio por noche (€)",
              y = NULL,
              caption = paste(
                "Mínimo 30 casos por grupo.",
                "El intervalo no recoge sesgos de cobertura."
              )
            ) +
            tema()
          
          # EJERCICIO 17 --------------------------------------------------------
          
        } else if (numero == 17) {
          
          hosts <- d %>%
            filter(!is.na(host_id)) %>%
            count(
              region_origen,
              host_id,
              name = "alojamientos"
            ) %>%
            group_by(region_origen) %>%
            mutate(
              cuota = alojamientos / sum(alojamientos)
            ) %>%
            ungroup()
          
          validate(
            need(
              nrow(hosts) > 0,
              "No hay anfitriones identificados."
            )
          )
          
          r <- hosts %>%
            group_by(region_origen) %>%
            summarise(
              HHI = 10000 * sum(cuota^2),
              Cuota_top10 = 100 * sum(
                head(
                  sort(cuota, decreasing = TRUE),
                  10
                )
              ),
              Anfitriones = n(),
              .groups = "drop"
            )
          
          visual <- r %>%
            pivot_longer(
              c(HHI, Cuota_top10),
              names_to = "Metrica",
              values_to = "Valor"
            )
          
          g <- ggplot(
            visual,
            aes(
              Valor,
              reorder(region_origen, Valor)
            )
          ) +
            geom_col(
              fill = "#14B8A6",
              width = 0.65
            ) +
            facet_wrap(
              ~Metrica,
              scales = "free_x"
            ) +
            labs(
              title = "17. Concentración de anfitriones",
              x = NULL,
              y = NULL,
              caption = paste(
                "HHI = suma de cuotas al cuadrado × 10.000.",
                "Mayor HHI indica mayor concentración en la selección."
              )
            ) +
            tema()
          
          # EJERCICIO 19 --------------------------------------------------------
          
        } else if (numero == 19) {
          
          dd <- d %>%
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
          
          validate(
            need(
              nrow(dd) >= 30,
              "Necesitas al menos 30 casos completos para segmentar."
            )
          )
          
          variables <- dd %>%
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
              function(x) {
                is.finite(sd(x)) && sd(x) > 0
              },
              logical(1)
            )
          ]
          
          validate(
            need(
              ncol(variables) >= 2,
              "No hay suficientes variables con variación."
            )
          )
          
          X <- scale(variables)
          
          max_k <- min(
            6,
            nrow(unique(as.data.frame(X))) - 1
          )
          
          validate(
            need(
              max_k >= 2,
              "No hay suficientes perfiles diferentes."
            )
          )
          
          ks <- 2:max_k
          
          modelos <- lapply(ks, function(k) {
            
            set.seed(1900 + k)
            
            kmeans(
              X,
              centers = k,
              nstart = 25,
              iter.max = 100
            )
          })
          
          dispersion <- vapply(
            modelos,
            function(m) m$tot.withinss,
            numeric(1)
          )
          
          criterio <- nrow(X) * log(
            pmax(
              dispersion,
              .Machine$double.eps
            ) / nrow(X)
          ) +
            ks * ncol(X) * log(nrow(X))
          
          mejor <- which.min(criterio)
          
          dd$Segmento <- factor(
            modelos[[mejor]]$cluster
          )
          
          r <- dd %>%
            group_by(Segmento) %>%
            summarise(
              Alojamientos = n(),
              Precio_mediano = median(price),
              Capacidad = mean(accommodates),
              Dormitorios = mean(bedrooms),
              Nota = mean(review_scores_rating),
              Resenas = median(number_of_reviews),
              .groups = "drop"
            )
          
          visual <- as_tibble(X) %>%
            mutate(Segmento = dd$Segmento) %>%
            group_by(Segmento) %>%
            summarise(
              across(everything(), mean),
              .groups = "drop"
            ) %>%
            pivot_longer(
              -Segmento,
              names_to = "Variable",
              values_to = "Valor"
            )
          
          g <- ggplot(
            visual,
            aes(Variable, Segmento, fill = Valor)
          ) +
            geom_tile(
              color = "white",
              linewidth = 2
            ) +
            geom_text(
              aes(label = sprintf("%+.2f", Valor)),
              size = 4
            ) +
            scale_fill_gradient2(
              low = "#99F6E4",
              mid = "#F8FAFC",
              high = "#C4B5FD",
              midpoint = 0
            ) +
            labs(
              title = "19. Segmentación automática",
              subtitle = paste(
                ks[mejor],
                "segmentos · selección heurística de K"
              ),
              x = NULL,
              y = "Segmento",
              caption = paste(
                "Valores estandarizados.",
                "Los segmentos no son un ranking."
              )
            ) +
            tema()
          
          # EJERCICIOS 18 Y 20 --------------------------------------------------
          
        } else {
          
          dd <- d %>%
            filter(
              is.finite(accommodates),
              accommodates > 0,
              is.finite(bedrooms),
              bedrooms >= 0,
              review_scores_rating > 0,
              review_scores_rating <= 5,
              is.finite(number_of_reviews),
              number_of_reviews >= 0,
              host_is_superhost %in% c("t", "f")
            )
          
          validate(
            need(
              nrow(dd) >= 100,
              "Necesitas al menos 100 casos completos para el modelo."
            )
          )
          
          set.seed(1801)
          
          dd <- dd %>%
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
                    sample.int(
                      n(),
                      floor(n() * 0.25)
                    )
                  ] <- TRUE
                }
                
                indicador
              }
            ) %>%
            ungroup()
          
          entrenamiento <- dd %>%
            filter(!test)
          
          prueba <- dd %>%
            filter(test)
          
          validate(
            need(
              nrow(prueba) >= 10,
              "La selección no permite reservar suficientes casos para validar."
            )
          )
          
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
          
          validate(
            need(
              any(usar),
              "No hay predictores con variación."
            )
          )
          
          formula_modelo <- as.formula(
            paste(
              "log(price) ~",
              paste(
                terminos[usar],
                collapse = " + "
              )
            )
          )
          
          modelo <- lm(
            formula_modelo,
            data = entrenamiento
          )
          
          prueba$estimado <- exp(
            predict(
              modelo,
              newdata = prueba
            )
          ) * mean(exp(residuals(modelo)))
          
          prueba <- prueba %>%
            filter(
              is.finite(estimado),
              estimado > 0
            ) %>%
            mutate(
              error = price - estimado,
              diferencia_pct =
                100 * (price / estimado - 1)
            )
          
          validate(
            need(
              nrow(prueba) > 0,
              "No se han obtenido predicciones válidas."
            )
          )
          
          if (numero == 18) {
            
            referencia <- entrenamiento %>%
              group_by(region_origen, room_type) %>%
              summarise(
                precio_base = median(price),
                .groups = "drop"
              )
            
            prueba <- prueba %>%
              left_join(
                referencia,
                by = c(
                  "region_origen",
                  "room_type"
                )
              )
            
            denominador <- sum(
              (prueba$price - mean(prueba$price))^2
            )
            
            r <- tibble(
              MAE = mean(abs(prueba$error)),
              RMSE = sqrt(mean(prueba$error^2)),
              R2 = if (denominador > 0) {
                1 - sum(prueba$error^2) / denominador
              } else {
                NA_real_
              },
              MAE_referencia = mean(
                abs(
                  prueba$price -
                    prueba$precio_base
                )
              ),
              Casos_entrenamiento = nrow(entrenamiento),
              Casos_validacion = nrow(prueba)
            )
            
            limite <- unname(
              quantile(
                c(
                  prueba$price,
                  prueba$estimado
                ),
                0.99
              )
            )
            
            g <- ggplot(
              prueba,
              aes(estimado, price)
            ) +
              geom_point(
                color = "#4F46E5",
                alpha = 0.25
              ) +
              geom_abline(
                slope = 1,
                intercept = 0,
                color = "#14B8A6",
                linetype = 2
              ) +
              coord_fixed(
                xlim = c(0, limite),
                ylim = c(0, limite)
              ) +
              labs(
                title = "18. Validación del modelo",
                subtitle = sprintf(
                  "MAE %.2f € · RMSE %.2f € · R² %.3f · MAE referencia %.2f €",
                  r$MAE,
                  r$RMSE,
                  r$R2,
                  r$MAE_referencia
                ),
                x = "Precio estimado (€)",
                y = "Precio anunciado (€)",
                caption = paste(
                  "Validación aleatoria, no temporal ni por anfitrión.",
                  "Zoom visual al percentil 99;",
                  "métricas sobre todos los casos de validación."
                )
              ) +
              tema()
            
          } else {
            
            r <- prueba %>%
              filter(
                review_scores_rating >= 4.8,
                number_of_reviews >= 50,
                diferencia_pct <= -20
              ) %>%
              arrange(diferencia_pct) %>%
              slice_head(n = 10)
            
            validate(
              need(
                nrow(r) > 0,
                paste(
                  "El modelo se ha calculado, pero ningún caso",
                  "cumple nota ≥ 4,8, reseñas ≥ 50",
                  "y precio al menos 20 % inferior al estimado."
                )
              )
            )
            
            r <- r %>%
              mutate(
                etiqueta = paste0(
                  row_number(), ". ",
                  str_trunc(
                    coalesce(name, "Sin nombre"),
                    32
                  )
                ),
                etiqueta = factor(
                  etiqueta,
                  levels = rev(etiqueta)
                )
              )
            
            g <- ggplot(
              r,
              aes(y = etiqueta)
            ) +
              geom_segment(
                aes(
                  x = price,
                  xend = estimado,
                  yend = etiqueta
                ),
                color = "#CBD5E1",
                linewidth = 2
              ) +
              geom_point(
                aes(
                  x = price,
                  color = "Anunciado"
                ),
                size = 4
              ) +
              geom_point(
                aes(
                  x = estimado,
                  color = "Estimado"
                ),
                size = 4,
                shape = 18
              ) +
              scale_color_manual(
                values = c(
                  Anunciado = "#14B8A6",
                  Estimado = "#8B5CF6"
                )
              ) +
              labs(
                title = "20. Precio real frente a estimado",
                x = "Precio por noche (€)",
                y = NULL,
                caption = paste(
                  "Solo casos reservados para validación.",
                  "Lista para investigar, no prueba de",
                  "infravaloración o rentabilidad."
                )
              ) +
              tema()
          }
        }
        
        incProgress(0.85)
        
        list(
          numero = numero,
          tabla = r,
          grafico = g,
          firma = firma,
          filas = nrow(d),
          fecha = Sys.time()
        )
      }
    )
    
  }, ignoreInit = TRUE)
  
  # Detectar si el resultado avanzado corresponde a los filtros actuales.
  firma_filtros <- reactive({
    
    list(
      version = version_datos(),
      ciudad = sort(input$ciudad),
      tipo = sort(input$tipo),
      precio = input$precio,
      capacidad = input$capacidad,
      dormitorios = input$dormitorios,
      nota = input$nota,
      resenas = input$resenas,
      superhost = input$superhost
    )
  })
  
  output$estado_avanzado <- renderUI({
    
    if (input$calcular == 0) {
      
      return(
        p(
          class = "nota",
          "Selecciona un análisis y pulsa «Calcular análisis»."
        )
      )
    }
    
    a <- analisis()
    
    desactualizado <- !identical(
      a$firma,
      firma_filtros()
    ) ||
      a$numero != as.integer(input$avanzado)
    
    if (desactualizado) {
      
      div(
        class = "alert alert-warning",
        paste(
          "Has cambiado los filtros o el análisis.",
          "El resultado mostrado corresponde al cálculo anterior.",
          "Pulsa «Calcular análisis» para actualizarlo."
        )
      )
      
    } else {
      
      p(
        class = "nota",
        paste(
          "Ejercicio", a$numero,
          "calculado sobre", a$filas,
          "alojamientos con ID único.",
          "Hora:",
          format(a$fecha, "%H:%M:%S")
        )
      )
    }
  })
  
  output$grafico_avanzado <- renderPlot({
    
    req(input$calcular > 0)
    
    analisis()$grafico
  })
  
  output$tabla_avanzada <- renderDT({
    
    req(input$calcular > 0)
    
    datatable(
      analisis()$tabla,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  output$descargar_4k <- downloadHandler(
    
    filename = function() {
      
      req(input$calcular > 0)
      
      paste0(
        "airbnb_ejercicio_",
        analisis()$numero,
        "_4K.png"
      )
    },
    
    content = function(file) {
      
      req(input$calcular > 0)
      
      ggsave(
        filename = file,
        plot = analisis()$grafico,
        device = "png",
        width = 12.8,
        height = 7.2,
        units = "in",
        dpi = 300,
        bg = "white"
      )
    }
  )
}

# ==============================================================================
# ABRIR LA APLICACIÓN
# ==============================================================================

shinyApp(
  ui = ui,
  server = server,
  options = list(
    launch.browser = TRUE
  )
)