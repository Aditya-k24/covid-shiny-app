library(shiny)
library(tidyverse)

# Load dataset
covid_data <- read.csv("data/country_wise_latest.csv")

# UI
ui <- fluidPage(
  titlePanel("🌍 COVID-19 Summary by Country"),

  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Choose a Country:",
                  choices = covid_data$`Country.Region`)
    ),

    mainPanel(
      h4("COVID-19 Summary"),
      tableOutput("summary_table"),
      plotOutput("bar_plot")
    )
  )
)

# Server
server <- function(input, output) {
  
  selected_country_data <- reactive({
    covid_data %>% filter(`Country.Region` == input$country)
  })
  
  output$summary_table <- renderTable({
    df <- selected_country_data()
    df %>% select(Confirmed, Deaths, Recovered, Active)
  })
  
  output$bar_plot <- renderPlot({
    df <- selected_country_data() %>%
      select(Confirmed, Deaths, Recovered, Active) %>%
      pivot_longer(cols = everything(), names_to = "Metric", values_to = "Count")
    
    ggplot(df, aes(x = Metric, y = Count, fill = Metric)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = paste("COVID-19 Stats for", input$country),
           x = NULL, y = "Count")
  })
}

# Launch the app
shinyApp(ui = ui, server = server)
