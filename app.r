library(shiny)
library(tidyverse)
data <- read_csv("population.csv")

ui <- fluidPage(

    titlePanel("Population in each country"),

    mainPanel(
      tabsetPanel(
        
        tabPanel("General information",
                 p("This set of data is the population of each country in ", em('1995-2013'), 
                   "This dataset has ", nrow(data), "rows and", ncol(data), 
                   "columns. I will focus on the population trend of ", strong("China, Japan, and Germany"
                 ))
                   
                 
        ),
        
        tabPanel("Plot",
                 sidebarLayout(
                   sidebarPanel(
                     radioButtons("country", label = "Choose country",
                                  choices = list("China" = "China", 
                                                 "Japan" = "Japan",
                                                 "Germany" = "Germany")),
                     
                     radioButtons("color", label = "Line color", 
                                 choices = list("Black" = "black", 
                                                "Red" = "red",
                                                "Gold" = "gold"))
                     
                   ),
                   mainPanel(plotOutput("country_plot"),
                             textOutput("plot_text"))
                 )),
        
        tabPanel("Table",
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput("year", label = "Choose the time",
                                 min = min(data$year),
                                 max = max(data$year),
                                 value = 2010)
                     ),
                   mainPanel(dataTableOutput("country_table"),
                             textOutput("table_text")
                   )
                 )),
        )
      ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$country_plot <- renderPlot({
    data %>% 
      filter(country %in% input$country) %>%
      ggplot(aes(year, population)) + 
      geom_line(col = input$color) + 
      labs(x = "year", y = "Population")
  })
  
  output$plot_text <- renderText({
    text_plot <- data %>% 
      filter(country %in% input$country) %>%
      arrange(desc(population))
    paste("The largest number of people in ", text_plot$country[1], "is ",
          text_plot$population[1])
  })
  
  output$country_table <- renderDataTable({
      country_table <- data %>% 
        filter(year == input$year)
  })
  
  output$table_text <- renderText({
    text_table <- data %>% 
      filter(year %in% input$year) %>%
      arrange(desc(population))
    paste("In ", text_table$year[1], ", the largest number of people is in ", text_table$country[1], ", with ",
          text_table$population[1], "people.")
  })
  
}

shinyApp(ui = ui, server = server)
