# Cargar librerías necesarias
library(shiny)
library(dplyr)
library(plotly)
library(readxl)

# Cargar los datos
datos <- datos_filtrados
datos$FechaInicioTrimestre <- as.Date(datos$FechaInicioTrimestre)

# Crear una nueva columna 'Año' extraída de la fecha
datos <- datos %>%
  mutate(Año = format(FechaInicioTrimestre, "%Y"))

# Calcular la suma de 'Cantidad' por 'Año' y 'Contenidos'
totales_por_ano <- datos %>%
  group_by(Año, Contenidos) %>%
  summarise(Total = sum(Cantidad), .groups = 'drop')  # Controlar el agrupamiento

# Obtener la lista de años disponibles
lista_anos <- unique(totales_por_ano$Año)
lista_anos <- sort(lista_anos)  # Asegurarse de que los años estén ordenados

# Definir la UI de la aplicación
ui <- fluidPage(
  titlePanel("Contenidos negociados por año"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("rangoAnos", 
                  "Seleccione el rango de años:", 
                  min = as.numeric(min(lista_anos)), 
                  max = as.numeric(max(lista_anos)), 
                  value = c(as.numeric(min(lista_anos)), as.numeric(max(lista_anos))),
                  step = 1,
                  animate = TRUE),
      
      # Botón para descargar el dataset
      downloadButton("downloadData", "Descargar Dataset")
    ),
    
    mainPanel(
      plotlyOutput("barPlot")
    )
  ),
  
  # Incluir el código JavaScript para cambiar el formato del slider
  tags$script(HTML("
    $(document).on('shiny:inputchanged', function(event) {
      if (event.name === 'rangoAnos') {
        var slider = $('#' + event.name);
        slider.on('change', function() {
          var values = $(this).val();
          var formattedValues = values.map(function(value) {
            return value.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, '.');
          });
          slider.attr('data-slider-tooltip-split', formattedValues.join(' - '));
        });
      }
    });
  "))
)

# Definir el servidor
server <- function(input, output) {
  
  # Filtrar los datos según el rango de años seleccionado
  datos_filtrados <- reactive({
    data <- totales_por_ano
    
    # Convertir el rango de años a formato numérico
    año_min <- as.character(input$rangoAnos[1])
    año_max <- as.character(input$rangoAnos[2])
    
    # Filtrar por el rango de años
    data <- data %>% filter(Año >= año_min & Año <= año_max)
    
    return(data)
  })
  
  # Crear el gráfico y renderizarlo
  output$barPlot <- renderPlotly({
    filtered_data <- datos_filtrados()
    
    plot_ly(
      data = filtered_data,
      x = ~Año,
      y = ~Total,
      color = ~Contenidos,
      type = 'bar',
      barmode = 'dodge'
    ) %>%
      layout(
        title = "Total por Año",
        xaxis = list(title = "Año"),
        yaxis = list(title = "Total")
      )
  })
  
  # Definir la función de descarga del dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("dataset_totales_por_ano", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datos_filtrados(), file, row.names = FALSE)
    }
  )
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
