library(tidyverse)
library(readxl)
library(here)
df <- read_excel(here("data/slu_graduates_17_21.xlsx"))

## fixes error in the data
df <- df %>% mutate(across(everything(),
                           .fns = ~replace(., . ==  "STATS" , "STAT")))

df_long <- df %>% pivot_longer(3:8, names_to = "type", values_to = "discipline")
df_major <- df_long %>% 
  filter(type == "major1" | type == "major2" | type == "major3")

df_stat <- df_major %>% filter(discipline == "STAT") 
df_statfull <- semi_join(df_long, df_stat, by = "adm_id") %>%
  filter(type == "major1" |
           type == "major2" | 
           type == "major3")

df_nostat <- df_statfull %>% filter(discipline != "STAT" &
                                      !is.na(discipline)) %>%
  group_by(discipline) %>%
  summarise(nstudent = n()) %>%
  mutate(discipline = fct_reorder(discipline, nstudent))
ggplot(data = df_nostat, aes(x = discipline, y = nstudent)) +
  geom_col() +
  coord_flip()

library(shiny)

df$major1

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "majorchoice",  
        label = "Choose a Major", 
        choices = df$major1)
    ),
    mainPanel(
      plotOutput(outputId = "majorplot")
    )
  )
)

server <- function(input, output, session) {
  
  df_update <- reactive({
    df_discipline <- 
      df_major %>% filter(discipline == input$majorchoice) 
    
    df_disciplinefull <-
      semi_join(df_long, df_discipline, by = "adm_id") %>%
      filter(type == "major1" |
               type == "major2" | 
               type == "major3")
    
    df_nodiscipline <- 
      df_disciplinefull %>% filter(discipline != input$majorchoice &
                                       !is.na(discipline)) %>%
      group_by(discipline) %>%
      summarise(nstudent = n()) %>%
      mutate(discipline = fct_reorder(discipline, nstudent))
  })
  
  
  output$majorplot <- renderPlot({
    ggplot(data = df_update(), aes(x = discipline, y = nstudent)) +
      geom_col() +
      coord_flip()
  })  
}

shinyApp(ui, server)
