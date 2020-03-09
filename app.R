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
      fileInput("file1", "Elija su archivo json",
                multiple = FALSE,
                accept = c("application/json",
                         ".json")),

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
    ifelse(x < 13 , "green",
    ifelse(x > 13 & x <= 25 , "yellow",
    ifelse( x > 25 & x <= 35, "orange",
    ifelse( x > 35 & x <= 55, "red",
    ifelse( x > 55 & x <= 100, "purple",
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
            addCircleMarkers(~lon, ~lat, popup = ~as.character(P25), color = ~color, layerId = ~timestamp)
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
