library(tidyverse)
library(here)
poke_df <- read_csv(here("data/pokemon_full.csv"))

poke_long <- poke_df %>% pivot_longer(4:9, values_to = "value", 
                                      names_to = "stat")

poke_small <- poke_long %>% filter(Name == "Bulbasaur" | Name == "Ivysaur")
ggplot(data = poke_small, aes(x = stat, y = value)) +
  geom_col(aes(fill = Name), position = "dodge")


library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectizeInput("pokechoose1", label = "Choose Pokemon",
                     choices = poke_df$Name, selected = "Bulbasaur",
                     multiple = TRUE),
      actionButton("runappbutton", label = "Update Stats")
    ),
    mainPanel(
      plotOutput(outputId = "pokegraph")
    )
  )
)

server <- function(input, output, session) {
  
  poke_react <-  eventReactive(input$runappbutton, {
    poke_long %>% filter(Name %in% input$pokechoose1)
  })
  
  output$pokegraph <- renderPlot({
    
    ggplot(data = poke_react(), aes(x = stat, y = value)) +
      geom_col(aes(fill = Name), position = "dodge") +
      coord_flip() +
      scale_fill_viridis_d()
  }
  )
  
}

shinyApp(ui, server)

shinyApp(ui, server)