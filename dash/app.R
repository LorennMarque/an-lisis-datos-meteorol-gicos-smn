library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(leaflet)
library(dplyr)
library(plotly)
library(RColorBrewer)
library(sf)
library(shinycssloaders)
library(data.table)
library(lubridate)
library(viridis)  # Nueva librería para mejores paletas de color

# Cargar datos de estaciones desde un archivo CSV usando fread para una lectura más rápida
estaciones <- fread("../data/estaciones.csv")

# Verificar que las columnas necesarias existan
required_columns <- c("Nombre", "Provincia", "Altura", "Nro", "Latitud", "Longitud")
if (!all(required_columns %in% colnames(estaciones))) {
  stop(paste("El archivo estaciones.csv debe contener las columnas:", paste(required_columns, collapse = ", ")))
}

# Cargar datos limpios desde un archivo RDS y convertirlos a data.table
clean_data <- readRDS("../clean_data.rds")
setDT(clean_data)

# Asegurar que 'fecha' sea de tipo Date y agregar columna 'year'
clean_data[, fecha := as.Date(fecha)]
clean_data[, year := year(fecha)]

# Obtener las fechas mínima y máxima del conjunto de datos
fecha_min <- min(clean_data$fecha, na.rm = TRUE)
fecha_max <- max(clean_data$fecha, na.rm = TRUE)

# Cargar el archivo GeoJSON de las provincias argentinas
provincias_geojson <- sf::st_read("../rasters/provincias.json")  # Reemplaza con la ruta correcta

provincias_geojson <- provincias_geojson %>%
  mutate(
    nam = toupper(iconv(nam, to = "ASCII//TRANSLIT")),
    nam = case_when(
      nam == "TIERRA DEL FUEGO, ANTARTIDA E ISLAS DEL ATLANTICO SUR" ~ "TIERRA DEL FUEGO",
      TRUE ~ nam
    )
  )

ui <- dashboardPage(
  dashboardHeader(title = "Datos SMN"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Estaciones", tabName = "estaciones", icon = icon("cloud")),
      menuItem("Precipitaciones", tabName = "precipitaciones", icon = icon("tint")),
      menuItem("General", tabName = "general", icon = icon("chart-line")),
      menuItem("Temperaturas", tabName = "temperaturas", icon = icon("thermometer-half"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "estaciones",
              h2("Estaciones Meteorológicas"),
              selectInput("filtro_estaciones", "Selecciona una estación:", 
                          choices = c("Todas", unique(estaciones$Nombre))),
              leafletOutput("mapa_estaciones", height = "500px") %>% withSpinner(color="#0dc5c1"),
              dataTableOutput("tabla_estaciones") %>% withSpinner(color="#0dc5c1")
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
                column(6, plotlyOutput("grafico_barras_acumuladas") %>% withSpinner(color="#0dc5c1")),
                column(6, plotlyOutput("grafico_boxplot") %>% withSpinner(color="#0dc5c1"))
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
        plotlyOutput("grafico_general") %>% withSpinner(color="#0dc5c1"),
        dataTableOutput("tabla_general") %>% withSpinner(color="#0dc5c1")
      ),
      tabItem(
        tabName = "temperaturas",
        h2("Temperaturas Promedio por Provincia"),
        sliderInput("year_selector", "Selecciona el año:", 
                    min = as.numeric(format(fecha_min, "%Y")), 
                    max = as.numeric(format(fecha_max, "%Y")), 
                    value = as.numeric(format(fecha_min, "%Y")), 
                    step = 1,
                    sep = ""),
        fluidRow(
          column(width = 6, leafletOutput("mapa_temperaturas", height = "500px") %>% withSpinner(color="#0dc5c1")),
          column(width = 6, plotlyOutput("grafico_barras_temperaturas", height = "500px") %>% withSpinner(color="#0dc5c1"))
        )
      )
    )
  )
)

server <- function(input, output) {
  # Filtrar estaciones según la selección usando data.table
  estaciones_filtradas <- reactive({
    if (input$filtro_estaciones == "Todas") {
      estaciones
    } else {
      estaciones[Nombre == input$filtro_estaciones]
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
    clean_data[
      provincia %in% input$provincia_precipitaciones &
        fecha >= input$rango_fechas[1] &
        fecha <= input$rango_fechas[2],
      .(precipitacion_diaria = sum(precipitacion_mm, na.rm = TRUE)),
      by = .(provincia, fecha)
    ][order(provincia, fecha)]  # Ordenar los datos
  })
  
  # Renderizar el gráfico de precipitaciones con plotly
  output$grafico_precipitaciones <- renderPlotly({
    datos <- datos_filtrados()
    num_provincias <- length(unique(datos$provincia))
    colores <- viridis(num_provincias)
    plot_ly(data = datos, x = ~fecha, y = ~precipitacion_diaria, type = 'scatter', mode = 'lines',
            color = ~provincia, colors = colores) %>%
      layout(title = "Precipitación diaria por provincia",
             xaxis = list(title = "Fecha"),
             yaxis = list(title = "Precipitación (mm)"),
             legend = list(title = list(text = "Provincias")))
  })
  
  # Gráfico de barras apiladas de precipitación acumulada por provincia
  output$grafico_barras_acumuladas <- renderPlotly({
    datos <- datos_filtrados()[,
                               .(precipitacion_acumulada = sum(precipitacion_diaria, na.rm = TRUE)),
                               by = provincia]
    datos <- datos[order(-precipitacion_acumulada)]  # Ordenar de mayor a menor
    plot_ly(data = datos, x = ~reorder(provincia, -precipitacion_acumulada), y = ~precipitacion_acumulada, 
            type = 'bar', name = 'Precipitación Acumulada', marker = list(color = viridis(nrow(datos)))) %>%
      layout(title = "Precipitación Acumulada por Provincia",
             xaxis = list(title = "Provincia"),
             yaxis = list(title = "Precipitación Acumulada (mm)"))
  })
  
  # Gráfico de caja y bigotes de distribución de precipitaciones diarias por provincia
  output$grafico_boxplot <- renderPlotly({
    datos <- datos_filtrados()
    num_provincias <- length(unique(datos$provincia))
    colores <- viridis(num_provincias)
    plot_ly(data = datos, y = ~precipitacion_diaria, color = ~provincia, colors = colores, type = 'box') %>%
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
    
    datos <- clean_data[
      provincia %in% input$provincia_general &
        fecha >= input$rango_fechas_general[1] &
        fecha <= input$rango_fechas_general[2]
    ]
    
    datos[, periodo := switch(
      input$frecuencia_general,
      "weekly" = paste0(year(fecha), "-W", sprintf("%02d", isoweek(fecha))),
      "monthly" = format(fecha, "%Y-%m"),
      "yearly" = as.character(year(fecha)),
      as.character(fecha)
    )]
    
    datos <- datos[, .(valor = mean(get(input$caracteristica_general), na.rm = TRUE)),
                   by = .(provincia, periodo)]
    datos <- datos[order(provincia, periodo)]  # Ordenar los datos
    datos
  })
  
  # Renderizar el gráfico en la pestaña General
  output$grafico_general <- renderPlotly({
    datos <- datos_general()
    num_provincias <- length(unique(datos$provincia))
    colores <- viridis(num_provincias)
    
    plot_ly(data = datos, x = ~periodo, y = ~valor, type = 'scatter', mode = 'lines',
            color = ~provincia, colors = colores) %>%
      layout(title = paste("Evolución de", names(which(c(
        "temp" = "Temperatura",
        "hum" = "Humedad",
        "pnm" = "Presión",
        "ff" = "Velocidad del viento",
        "precipitacion_mm" = "Precipitación"
      ) == input$caracteristica_general)), "por provincia"),
      xaxis = list(title = "Fecha", type = "category"),
      yaxis = list(title = names(which(c(
        "temp" = "Temperatura (°C)",
        "hum" = "Humedad (%)",
        "pnm" = "Presión (hPa)",
        "ff" = "Velocidad del viento (km/h)",
        "precipitacion_mm" = "Precipitación (mm)"
      ) == input$caracteristica_general))),
      legend = list(title = list(text = "Provincias")))
  })
  
  # Renderizar la tabla de datos en la pestaña General
  output$tabla_general <- renderDataTable({
    datatable(datos_general(), options = list(pageLength = 10))
  })
  
  temperaturas_provincia <- reactive({
    clean_data[year == input$year_selector,
               .(temp_promedio = mean(temp, na.rm = TRUE)),
               by = provincia]
  })
  
  # Renderizar el mapa
  output$mapa_temperaturas <- renderLeaflet({
    # Evaluar la expresión reactiva antes de la unión
    temperaturas_data <- temperaturas_provincia()
    
    # Realizar la unión con el GeoJSON
    provincias_datos <- provincias_geojson %>%
      left_join(temperaturas_data, by = c("nam" = "provincia"))
    
    # Definir la paleta de colores
    paleta_colores <- colorNumeric(palette = "plasma", domain = provincias_datos$temp_promedio, na.color = "transparent")
    
    # Crear el mapa
    leaflet(provincias_datos) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~paleta_colores(temp_promedio),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste(nam, "<br>Temperatura Promedio:", round(temp_promedio, 2), "°C"),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = paleta_colores,
        values = ~temp_promedio,
        opacity = 0.7,
        title = "Temperatura Promedio (°C)",
        position = "bottomright"
      )
  })
  
  # Renderizar el gráfico de barras
  output$grafico_barras_temperaturas <- renderPlotly({
    datos <- temperaturas_provincia()[order(temp_promedio, decreasing = TRUE)]
    plot_ly(
      data = datos,
      x = ~reorder(provincia, temp_promedio),
      y = ~temp_promedio,
      type = 'bar',
      marker = list(color = viridis(nrow(datos)))
    ) %>%
      layout(
        title = paste("Temperatura Promedio por Provincia en", input$year_selector),
        xaxis = list(title = "Provincia", tickangle = -45),
        yaxis = list(title = "Temperatura Promedio (°C)"),
        margin = list(b = 100),
        showlegend = FALSE
      )
  })
}

shinyApp(ui = ui, server = server)
