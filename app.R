library(shiny)
library(jsonlite)
library(tidyverse)
library(leaflet)
library(xts)
library(dygraphs)

options(shiny.maxRequestSize = 3 * 1024^2)

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Visor de rutas contaminación del aire"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a file ----
      fileInput("file1", "Elija su archivo json",
        multiple = FALSE,
        accept = c(
          "application/json",
          ".json"
        )
      ),

      # Horizontal line ----
      tags$hr(),
      selectInput(
        "pollulant", "Elija el tipo de contaminante a visualizar",
        choices = c("PM25", "CO2"),
        selectize = FALSE
      ),
      sliderInput("radius", "Tamaño del punto",
        min = 4, max = 30,
        value = 4
      ),
      a("visor desarrollado por un/loquer", href = "https://github.com/daquina-io/visor-canair.io"),
      tags$hr(),
      ## Button
      downloadButton("downloadCSVData", "Convertir archivo JSON a CSV")
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Data file ----
      leafletOutput("contents"),
      dygraphOutput("dygraph")
    )
  )
)

set_color_co2 <- function(x) {
  ifelse(x < 600, "green",
    ifelse(x >= 600 & x < 800, "yellow",
      ifelse(x >= 800 & x < 1000, "orange",
        ifelse(x >= 1000 & x < 1500, "red",
          ifelse(x >= 1500 & x < 2000, "purple",
            "maroon"
          )
        )
      )
    )
  )
}

set_color_p25 <- function(x) {
  ifelse(x < 13, "green",
    ifelse(x >= 13 & x < 35, "yellow",
      ifelse(x >= 35 & x < 55, "orange",
        ifelse(x >= 55 & x < 150, "red",
          ifelse(x >= 150 & x < 250, "purple",
            "maroon"
          )
        )
      )
    )
  )
}

set_color <- function(pollulant) {
  if (pollulant == "PM25") {
    return(set_color_p25)
  }
  return(set_color_co2)
}

## Define server logic to read selected file ----
server <- function(input, output, session) {
  df <- reactive({
    ## input$file1 will be NULL initially. After the user selects
    ## and uploads a file, head of that data file by default,
    ## or all rows if selected, will be shown.

    req(input$file1)

    ## when reading semicolon separated files,
    ## having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- fromJSON(input$file1$datapath)
      },
      error = function(e) {
        ## return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    df$data <- df$data %>% mutate(color = set_color(input$pollulant)(as.numeric(P25)))
    df$data$time <- as.POSIXct(df$data$timestamp, origin = "1970-01-01")

    df
  })

  output$contents <- renderLeaflet({
    leaflet(df()$data) %>%
      addTiles() %>%
      addCircleMarkers(~lon, ~lat, popup = ~ as.character(P25), color = ~color, layerId = ~timestamp, radius = ~ input$radius, stroke = FALSE, fillOpacity = 0.5)
  })

  output$dygraph <- renderDygraph({
    dygraph(xts(df()$data$P25, order.by = df()$data$time), main = sprintf("Exposición %s", input$pollulant), ylab = ifelse(input$pollulant == "PM25", "μg/m³", "ppm")) %>%
      dySeries(label = ifelse(input$pollulant == "PM25", "μg/m³", "ppm")) %>%
      dyRangeSelector()
  })

  # Downloadable csv of selected dataset ----
  output$downloadCSVData <- downloadHandler(
    filename = function() {
      paste(df()$name, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df()$data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
}

## Create Shiny app ----
shinyApp(ui, server)
