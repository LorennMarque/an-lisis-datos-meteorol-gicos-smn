library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(leaflet)
library(dplyr)
library(plotly)
library(RColorBrewer)

# Cargar datos de estaciones desde un archivo CSV
estaciones <- read_csv("../data/estaciones.csv")

# Verificar que las columnas necesarias existan
required_columns <- c("Nombre", "Provincia", "Altura", "Nro", "Latitud", "Longitud")
if (!all(required_columns %in% colnames(estaciones))) {
  stop(paste("El archivo estaciones.csv debe contener las columnas:", paste(required_columns, collapse = ", ")))
}

# Cargar datos limpios desde un archivo RDS
clean_data <- readRDS("../clean_data.rds")

# Obtener las fechas mínima y máxima del conjunto de datos
fecha_min <- min(clean_data$fecha, na.rm = TRUE)
fecha_max <- max(clean_data$fecha, na.rm = TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Datos SMN"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Estaciones", tabName = "estaciones", icon = icon("cloud")),
      menuItem("Precipitaciones", tabName = "precipitaciones", icon = icon("tint")),
      menuItem("General", tabName = "general", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "estaciones",
              h2("Estaciones Meteorológicas"),
              selectInput("filtro_estaciones", "Selecciona una estación:", 
                          choices = c("Todas", unique(estaciones$Nombre))),
              leafletOutput("mapa_estaciones", height = "500px"),
              dataTableOutput("tabla_estaciones")
      ),
      tabItem(tabName = "precipitaciones",
              h2("Precipitaciones"),
              selectInput("provincia_precipitaciones", "Selecciona una o más provincias:", 
                          choices = unique(clean_data$provincia),
                          selected = "MENDOZA",
                          multiple = TRUE,
                          selectize = TRUE),
              dateRangeInput("rango_fechas", "Selecciona el rango de fechas:",
                             start = fecha_min,
                             end = fecha_max,
                             min = fecha_min,
                             max = fecha_max,
                             format = "yyyy-mm-dd",
                             separator = " a "),
              plotlyOutput("grafico_precipitaciones"),
              br(),
              br(),
              fluidRow(
                column(6, plotlyOutput("grafico_barras_acumuladas")),
                column(6, plotlyOutput("grafico_boxplot"))
              ),
              dataTableOutput("tabla_datos_precipitaciones")
      ),
      tabItem(
        tabName = "general",
        h2("Análisis General"),
        fluidRow(
          column(
            width = 4,
            selectInput(
              "provincia_general",
              "Selecciona una o más provincias:",
              choices = unique(clean_data$provincia),
              selected = "MENDOZA",
              multiple = TRUE,
              selectize = TRUE
            )
          ),
          column(
            width = 4,
            selectInput(
              "caracteristica_general",
              "Selecciona una característica:",
              choices = c(
                "Temperatura" = "temp",
                "Humedad" = "hum",
                "Presión" = "pnm",
                "Velocidad del viento" = "ff",
                "Precipitación" = "precipitacion_mm"
              ),
              selected = "temp"
            )
          ),
          column(
            width = 4,
            selectInput(
              "frecuencia_general",
              "Selecciona la frecuencia de los datos:",
              choices = c(
                "Diario" = "daily",
                "Semanal" = "weekly",
                "Mensual" = "monthly",
                "Anual" = "yearly"
              ),
              selected = "daily"
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            dateRangeInput(
              "rango_fechas_general",
              "Selecciona el rango de fechas:",
              start = fecha_min,
              end = fecha_max,
              min = fecha_min,
              max = fecha_max,
              format = "yyyy-mm-dd",
              separator = " a "
            )
          )
        ),
        plotlyOutput("grafico_general"),
        dataTableOutput("tabla_general")
      )
      
    )
  )
)

server <- function(input, output) {
  # Filtrar estaciones según la selección
  estaciones_filtradas <- reactive({
    if (input$filtro_estaciones == "Todas") {
      estaciones
    } else {
      estaciones[estaciones$Nombre == input$filtro_estaciones, ]
    }
  })
  
  # Renderizar la tabla de estaciones
  output$tabla_estaciones <- renderDataTable({
    datatable(estaciones_filtradas(), options = list(pageLength = 5))
  })
  
  # Renderizar el mapa con las estaciones filtradas
  output$mapa_estaciones <- renderLeaflet({
    leaflet(estaciones_filtradas()) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitud,
        lat = ~Latitud,
        radius = 5,
        color = "blue",
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~paste0("<strong>Nombre:</strong> ", Nombre, "<br>",
                        "<strong>Provincia:</strong> ", Provincia, "<br>",
                        "<strong>Altura:</strong> ", Altura, " m<br>",
                        "<strong>Número:</strong> ", Nro, "<br>",
                        "<strong>Latitud:</strong> ", Latitud, "<br>",
                        "<strong>Longitud:</strong> ", Longitud)
      )
  })
  
  # Filtrar datos de precipitación según las provincias y el rango de fechas seleccionados
  datos_filtrados <- reactive({
    req(input$provincia_precipitaciones)  # Asegura que haya al menos una provincia seleccionada
    clean_data %>%
      filter(provincia %in% input$provincia_precipitaciones,
             fecha >= input$rango_fechas[1],
             fecha <= input$rango_fechas[2]) %>%
      group_by(provincia, fecha) %>%
      summarise(precipitacion_diaria = sum(precipitacion_mm, na.rm = TRUE), .groups = 'drop')
  })
  
  # Renderizar el gráfico de precipitaciones con plotly
  output$grafico_precipitaciones <- renderPlotly({
    datos <- datos_filtrados()
    colores <- brewer.pal(n = length(unique(datos$provincia)), name = "Set1")
    plot_ly() %>%
      add_trace(data = datos, x = ~fecha, y = ~precipitacion_diaria, type = 'scatter', mode = 'lines',
                color = ~provincia, colors = colores) %>%
      layout(title = "Precipitación diaria por provincia",
             xaxis = list(title = "Fecha"),
             yaxis = list(title = "Precipitación (mm)"),
             legend = list(title = list(text = "Provincias")))
  })
  # Gráfico de barras apiladas de precipitación acumulada por provincia
  output$grafico_barras_acumuladas <- renderPlotly({
    datos <- datos_filtrados() %>%
      group_by(provincia) %>%
      summarise(precipitacion_acumulada = sum(precipitacion_diaria, na.rm = TRUE))
    
    plot_ly(data = datos, x = ~provincia, y = ~precipitacion_acumulada, type = 'bar', name = 'Precipitación Acumulada') %>%
      layout(title = "Precipitación Acumulada por Provincia",
             xaxis = list(title = "Provincia"),
             yaxis = list(title = "Precipitación Acumulada (mm)"))
  })
  
  # Gráfico de caja y bigotes de distribución de precipitaciones diarias por provincia
  output$grafico_boxplot <- renderPlotly({
    datos <- datos_filtrados()
    
    plot_ly(data = datos, y = ~precipitacion_diaria, color = ~provincia, type = 'box') %>%
      layout(title = "Distribución de Precipitaciones Diarias por Provincia",
             yaxis = list(title = "Precipitación Diaria (mm)"),
             xaxis = list(title = "Provincia"))
  })
  
  # Tabla de datos de precipitaciones filtrados
  output$tabla_datos_precipitaciones <- renderDataTable({
    datos <- datos_filtrados()
    datatable(datos, options = list(pageLength = 10))
  })
  
  # Filtrar datos para la pestaña General
  datos_general <- reactive({
    req(input$provincia_general, input$caracteristica_general, input$frecuencia_general)
    
    datos <- clean_data %>%
      filter(provincia %in% input$provincia_general,
             fecha >= input$rango_fechas_general[1],
             fecha <= input$rango_fechas_general[2])
    
    datos <- datos %>%
      mutate(periodo = case_when(
        input$frecuencia_general == "weekly" ~ format(fecha, "%Y-%U"),
        input$frecuencia_general == "monthly" ~ format(fecha, "%Y-%m"),
        input$frecuencia_general == "yearly" ~ format(fecha, "%Y"),
        TRUE ~ as.character(fecha)
      )) %>%
      group_by(provincia, periodo) %>%
      summarise(valor = mean(.data[[input$caracteristica_general]], na.rm = TRUE), .groups = 'drop')
    
    datos
  })
  
  # Renderizar el gráfico en la pestaña General
  output$grafico_general <- renderPlotly({
    datos <- datos_general()
    colores <- brewer.pal(n = length(unique(datos$provincia)), name = "Set1")
    
    plot_ly(data = datos, x = ~periodo, y = ~valor, type = 'scatter', mode = 'lines',
            color = ~provincia, colors = colores) %>%
      layout(title = paste("Evolución de", input$caracteristica_general, "por provincia"),
             xaxis = list(title = "Fecha"),
             yaxis = list(title = input$caracteristica_general),
             legend = list(title = list(text = "Provincias")))
  })
  # Renderizar la tabla de datos en la pestaña General
  output$tabla_general <- renderDataTable({
    datatable(datos_general(), options = list(pageLength = 10))
  })
}

shinyApp(ui = ui, server = server)



