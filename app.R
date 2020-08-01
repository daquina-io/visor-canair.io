library(shiny)
library(jsonlite)
library(tidyverse)
library(leaflet)
library(xts)
library(dygraphs)


# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Visor de rutas - un/loquer"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a file ----
      fileInput("file1", "Elija su archivo de recorrido json",
                multiple = FALSE,
                accept = c("application/json",
                           ".json")),
      fileInput("file2", "Elija su archivo de anotaciones csv (creado en http://geojson.io/)",
                multiple = FALSE,
                accept = ".csv"),
      # Horizontal line ----
      tags$hr()

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Data file ----
        leafletOutput("contents"),
        dygraphOutput("dygraph")
    )

  )
)

set_color <- function(x) {
    ifelse( x < 13 , "green",
    ifelse( x >= 13 & x < 35 , "yellow",
    ifelse( x >= 35 & x < 55, "orange",
    ifelse( x >= 55 & x < 150, "red",
    ifelse( x >= 150 & x < 250, "purple",
           "maroon")))))
}



## Define server logic to read selected file ----
server <- function(input, output, session) {
    output$contents <-  renderLeaflet({
        ## input$file1 will be NULL initially. After the user selects
        ## and uploads a file, head of that data file by default,
        ## or all rows if selected, will be shown.
        req(input$file1)
        ## when reading semicolon separated files,
        ## having a comma separator causes `read.csv` to error
        tryCatch(
        {
            df <- fromJSON(input$file1$datapath)
            dfAnnotations <- read.csv(input$file2$datapath)
            ##dfAnnotations <- read.csv("~/Data/canairio/points.csv")
            dfAnnotations <- as.data.frame(dfAnnotations)
            colnames(dfAnnotations) <- c("color", "size", "symbol", "comment", "lng", "lat")
        },
        error = function(e) {
            ## return a safeError if a parsing error occurs
            stop(safeError(e))
        }
        )

        df$data <- df$data %>% mutate(color=set_color(as.numeric(P25)))
        df$data$time <- as.POSIXct(df$data$timestamp, origin="1970-01-01")

        leaflet(df$data) %>%
            addTiles() %>%
            addCircleMarkers(~lon, ~lat, popup = ~as.character(P25), color = ~color, layerId = ~timestamp)  %>%
            addMarkers(data = dfAnnotations, group = "Alertas", popup = ~comment)
    })

    output$dygraph <- renderDygraph({
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
        df$data$time <- as.POSIXct(df$data$timestamp, origin="1970-01-01")
        dygraph(xts(df$data$P25, order.by=df$data$time), main = "Exposición material particulado PM2.5", ylab = "μg/m³") %>%
            dySeries( label = "μg/m³") %>%
            dyRangeSelector()
    })
}

## Create Shiny app ----
shinyApp(ui, server)
