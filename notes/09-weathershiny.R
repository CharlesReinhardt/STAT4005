library(tidyverse)
library(lubridate)
library(here)

weather_df <- read_delim(here("data/canton_ny_weather_data.txt"))
weather_df

weather_df %>% 
  select(datetime, feelslike) %>%
  mutate(month = factor(month(datetime))) %>%
  filter(month == 12 | month == 4 | month == 7) %>%

  ggplot(aes(x = month, y = feelslike)) +
  geom_violin()

months <- c("Janurary", "Februrary", "March", "April", "May", "June", "July", 
            "August", "September", "October", "November", "December")

library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "monthrange",
              label = "Pick Some Months", min = 0, max = 12, 
              value = c(0, 12)),
  plotOutput("violin_plot")
)

server <- function(input, output, session) {
  new_weather <- reactive({
    weather_df %>% 
      select(datetime, feelslike) %>%
      mutate(month = factor(month(datetime))) %>%
      # The following line has some major problems
      filter(month %in% c(.data[[ input$monthrange[1] ]] : .data[[ input$monthrange[2] ]]))
  })
  
  output$violin_plot <- renderPlot({
    ggplot(data = new_weather(), aes(x = month, y = feelslike)) +
      geom_violin()
  })
}

shinyApp(ui, server)
