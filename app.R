# Load packages ----
library(shiny)
library(quantmod)
library(plotly)

# Source helpers ----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  # let's modify the CSS to change the chart design. 
  # well is for the sidebar, help-block is for the helpText, and datepicker is to modify the colors in the calendar. 
  tags$style(HTML("
    body {
      background-color: black;
      color: white;
    }
    
    .well {
      background-color: #171713;
      color: #ffffff;
    }
    
    .help-block {
      color: #ffffff;
    }
    
    .datepicker {
      background-color: #000000;
      color: #ffffff;
    }
    .datepicker table tr td,
    .datepicker table tr th {
      color: #ffffff;
      border-color: #ffffff;
    }
    .datepicker .active {
      background-color: #ffffff;
      color: #000000;
    }
  ")),
  (
    # this is the app title
  titlePanel("stockVis Interactive")),

  sidebarLayout(
    sidebarPanel(
      helpText("Data by: Yahoo Finance. This chart utilizes Posit stock example, but I've made it interactive by using Plotly. I've also added tryCatch and validate functions."),
      helpText("Type the ticker symbol to examine, e.g., SPY, MSFT, AAPL, etc."),
      textInput("symb", "Symbol", "SPY"),

      dateRangeInput("dates",
                     "Date range",
                     start = "2013-01-01",
                     end = as.character(Sys.Date())),

      br(),
      br(),

      checkboxInput("log", "Plot y axis on log scale",
                    value = FALSE),

      checkboxInput("adjust",
                    "Adjust prices for inflation", value = FALSE)
    ),

    mainPanel(plotlyOutput("plot"))
  )
)

# Server logic
server <- function(input, output, session) {
  
  dataInput <- reactive({
    validate(
      need(input$symb != "", "Please enter a ticker symbol.")
    )
    # We use tryCatch to detect non-existent ticker symbols
    tryCatch({
      getSymbols(input$symb, src = "yahoo",
                 from = input$dates[1],
                 to = input$dates[2],
                 auto.assign = FALSE)
    }, error = function(e) {
      # Custom error message for non-existent ticker symbols
      errorMessage <- "This ticker does not exist. You need the correct ticker symbol (e.g., GOOG instead of Google or Alphabet)."
      #the 2 lines below are error message for RStudio, but they don't work on shinyapps.io. you need shiny::showNotifications 
      #stop(errorMessage)
      #showNotification(errorMessage, type = "error")
      shiny::showNotification(errorMessage, type = "error")
    })
  })
  
  finalInput <- reactive({
    if (!input$adjust) return(dataInput())
    adjust(dataInput())
  })
  
  output$plot <- renderPlotly({
    data_xts <- finalInput()
    
    # Convert xts object to a data frame
    data_df <- data.frame(Date = index(data_xts), coredata(Cl(data_xts)))
    colnames(data_df) <- c("Date", "Price")
    
    # Create a plotly interactive plot
    plot_ly(data_df, x = ~Date, y = ~Price, type = "scatter", mode = "lines", line = list(color = '#a2823c')) %>%
      layout(title = toupper(input$symb),
             plot_bgcolor = '#000000',  # Black plot background
             paper_bgcolor = '#000000',  # Black paper background
             font = list(color = '#FFFFFF'),  # White font
             xaxis = list(gridcolor = '#37352d'),  # Modify x-axis grid color
             yaxis = list(gridcolor = '#37352d',  # Modify y-axis grid color
                          type = ifelse(input$log, "log", "linear")))
  })
  
}

# Run the app
shinyApp(ui, server)
