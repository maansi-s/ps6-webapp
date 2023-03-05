library(shiny)
library(tidyverse)
library(plotly)
file <- read_csv("seattle_01.csv")

ui <- fluidPage(
  
  titlePanel("Seattle AirBnb Listings: Which locations have more Airbnbs depending on party size?"),
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("n", "# of options you'd like to view at a time:",
                  min = 1,
                  max = 20,
                  value = 10),
      fluidRow(
        column(3,
               uiOutput("radioAccommodates")
        )
      ),
    ),
    mainPanel(
      textOutput("sentence"),
      tabsetPanel(
        tabPanel("About", htmlOutput("caption")),
        tabPanel("Plot", plotlyOutput("plot"), inline=TRUE),
        tabPanel("Table", tableOutput("table"))
      )
    )
  )
)

server <- function(input, output) {
  
  output$sentence <- renderText({ 
    s1 <- file %>%
      filter(accommodates %in% input$accommodates) 
    
    paste("The number of available AirBnbs with the given locations & accomodations is ", nrow(s1), ".")
  })
  
  output$caption <- renderText({
    HTML("The dataset used for this project is intended to help travelers book their ideal AirBnb when visiting the greater Seattle area.
         Information is given about pricings, number of bedrooms/bathrooms, ratings, location, and more.", "<br />", "<strong>", 
         "In this app, users may select how many people they are traveling with in order to find AirBnbs that can fit
         everyone in their party, and they can view which areas to search the most in according to how many AirBnb openings
         show up on the scatterplot and save time during their search for housing. They can also the maximum amount of 
         openings they'd like to see on the graph at a time.", "</strong>", "<br />",  
         "<em>", "View the following two tabs for more information on where to stay according to your housing needs.", "</em>")
  })
  
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
  
  output$table <- renderTable({
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
