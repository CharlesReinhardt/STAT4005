# Toy shiny to study 'tidy eval'

library(tidyverse)
toy_df <- tibble(xvar = c(2, 4, 5, 4),
                 yvar = c(4, 3, 2, 1), 
                 zvar = c("a", "b", "c", "d"))


toy_df

ggplot(data = toy_df, aes(x = "xvar")) + 
  geom_histogram()

env_var <- 3

toy_df %>% filter(.data[["xvar"]] < .env[[env_far]]) # .data[["varname"]] lets you do the stuff, .env[["varname"]] is in a similar vein, differentiates b/w data and environment


library(shiny)

ui <- fluidPage(
  radioButtons(inputId = "varchoices", 
               label = "Choose a Variable",
               choices = names(toy_df)),
  plotOutput("varhist")
)

server <- function(input, output, session) {
  output$varhist <- renderPlot({
    #ggplot(data = toy_df, aes_string(x = input$varchoices)) + # aes_string() is the easy fix if you're only working with ggplot2
    ggplot(data = toy_df, aes(x = .data[[input$varchoices]])) + # .data[["varname]] is the solution that works outside ggplot
      geom_histogram(color = "black", fill = "white")
  })
}

shinyApp(ui, server)