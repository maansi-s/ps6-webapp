library(shiny)
library(tidyverse)
library(rsconnect)
library(readr)
library(plotly)
file <- read_delim("seattle_01.csv")

ui <- fluidPage(
  
  tabsetPanel(
        tabPanel("About",
                  sidebarLayout(
                    sidebarPanel(
                      h1("Which locations have more Airbnbs depending on party size?"),
                      p(strong("The dataset used for this project is intended to help travelers book their ideal AirBnb when visiting the greater Seattle area.
                        Information is given about pricings, number of bedrooms/bathrooms, ratings, location, and more.")),
                      p("In this app, users may make selections regarding how many people they are traveling with, what type of room they'd like,
                        in what color they'd like to view the scatterplot, and how many options
                        they'd like to view at a time and save time during their search for housing."),
                      p(em("View the following two tabs for more information on where to 
                           stay according to your housing needs.")
                    ),
                    p("To the right is a sample of data: ")
                  ),
                  mainPanel(
                    tableOutput("frontpagetable")
                  )
                )
        ),
        
        tabPanel("Plot", 
                 sidebarLayout(
                   sidebarPanel(
                     p("The number of available AirBnbs with the given locations & accomodations is ", textOutput(outputId = "reactNumber", inline=T), "."),
                     fluidRow(column(6,
                                     radioButtons("accommodates", "How many people: ",
                                                  choices = (unique(file$accommodates)))
                       ),
                       column(6,
                              radioButtons("color", "Select a color: ",
                                           choices = c("skyblue","lawngreen","orange",
                                                                "purple","pink"))
                       )
                   )
                 ),
                 mainPanel(
                   plotOutput("plot")
                 )
            )
        ),
        tabPanel("Table",
                 sidebarLayout(
                   sidebarPanel(
                     p("The number of available AirBnbs with the select room type is ", textOutput(outputId = "reactNumber2", inline=T), "."),
                     sliderInput(
                       "n",
                       "# of options you'd like to view at a time:",
                       min = 1,
                       max = 20,
                       value = 10),
                     fluidRow(column(6,
                       radioButtons("room_type", "Select a room type: ",
                                    choices = (unique(file$room_type)))
                     ),
                     )
                   ),
                   mainPanel(tableOutput("table"))
       ))
  )
)

server <- function(input, output) {
  
  output$frontpagetable <- renderTable({
    table <- file %>%
      sample_n(5)
    print(table)
    table
  })
  
  output$reactNumber <- renderText({
    s1 <- file %>%
      filter(accommodates %in% input$accommodates) 
    if (nrow(s1) > input$n)
      sample_n(s1, input$n)
    else 
      s1
    
     return(nrow(s1))
  })
  
  output$reactNumber2 <- renderText({
    s1 <- file %>%
      filter(room_type %in% input$room_type) 
    if (nrow(s1) > input$n)
      sample_n(s1, input$n)
    else 
      s1
    
    return(nrow(s1))
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
      filter(room_type %in% input$room_type) 
    
    if (nrow(s1) > input$n)
        sample_n(s1, input$n)
    else 
      s1
  })
  
  output$plot <- renderPlot({
    p <- sample() %>%
      ggplot(aes(address, bedrooms)) +
      geom_point(col=input$color)
    if(nrow(sample()) == 0) {
      p <- p +
        labs(title = "Please select a color")
    }
    p
  })
  
}


shinyApp(ui = ui, server = server)
