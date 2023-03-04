library(shiny)
library(tidyverse)
library(plotly)
file <- read_csv("seattle_01.csv")

ui <- fluidPage(
  titlePanel("Seattle AirBnb Listings: Which locations have more Airbnbs depending on party size?"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Number of places:",
                  min = 1,
                  max = 20,
                  value = 10),
      fluidRow(
        column(6,
               uiOutput("radioAccommodates")
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotlyOutput("plot"))
        ##tabPanel("Summary", verbatimTextOutput("summary")),
        ##tabPanel("Table", tableOutput("table"))
      )
    )
  )
)

server <- function(input, output) {
  output$radioAccommodates <- renderUI({
    radioButtons("accommodates", "How many people: ",
                       choices = (unique(file$accommodates))
    )
  })
  sample <- reactive({
    s1 <- file %>%
      filter(accommodates %in% input$accommodates) 
    if (nrow(s1) > input$n)
      sample_n(s1, input$n)
    else 
      s1
  })
  
  output$plot <- renderPlotly({
    plot_ly(data = sample(),
            x = ~address, y = ~bedrooms, color = ~address, type = "scatter")
  })
}


shinyApp(ui = ui, server = server)