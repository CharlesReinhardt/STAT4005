# install.packages("broom")
slr_mod <- lm(ace ~ surface + tour + surface:tour, data = df)
summary(slr_mod)

## install.packages("broom")
## install.packages("lme4)
## install.packages("broom.mixed")
library(broom)
library(shiny)

ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    radioButtons(inputId = "modelchoice", label = "Choose a Model",
                 choices = c("Simple Reg", "Mixed")),
    radioButtons(inputId = "varchoice", label = "Choose a Variable",
                 choices = names(df_wta[5:9]))
    
  ),
  mainPanel(plotOutput("modelplot"))
)

)

server <- function(input, output, session) {
  
  output$modelplot <- renderPlot({
    
    if (input$modelchoice == "Simple Reg") {
      
      library(broom)
      
      simple_mod <- lm(df_wta[[input$varchoice]] ~ surface, data = df_wta)
      simple_aug <- augment(simple_mod)
      
      ggplot(data = simple_aug, aes(x = surface,
                                    y = `df_wta[[input$varchoice]]`)) +
        geom_jitter(alpha = 0.1, width = 0.3) +
        geom_point(aes(y = .fitted), colour = "red", size = 2) +
        labs(y = input$varchoice) +
        theme_minimal(base_size = 24)
      
    } else {
      library(lme4)
      library(broom.mixed)
      
      rand_int <- lmer(df_wta[[input$varchoice]] ~ surface +
                         (1 | player), data = df_wta)
      
      rand_int_aug <- augment(rand_int)
      
      ggplot(data = rand_int_aug, aes(x = surface,
                                      y = `df_wta[[input$varchoice]]`)) +
        facet_wrap(~ player) +
        geom_jitter(alpha = 0.1, width= 0.3) +
        geom_point(aes(y = .fitted), colour = "red")  +
        theme_minimal(base_size = 16) +
        labs(y = input$varchoice)
    }
    
  }, height = 1000, width = 1000
  )
  
}

shinyApp(ui, server)