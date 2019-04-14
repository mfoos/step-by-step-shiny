library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

ui <- fluidPage(
  
  fluidRow(
    column(width = 12,
           sliderInput("sample_count", "How many times should we sample each population?",
                       value = 10,
                       min = 1,
                       max = 500),
           plotOutput("sample_plot"),
           textOutput("sample_result")
    )
  )
)

server <- function(input, output) {
   
    sample_dat <- reactive({
      # Perform the random sampling and assign it to a reactive object
      # so when we plot and perform a t-test, they are not getting
      # different random samples
      data.frame("rowid" = paste0("row", seq(1,input$sample_count)),
                 "thicc bois" = rnorm(input$sample_count, 0, 1),
                 "chonkers" = rnorm(input$sample_count, 0.2, 1),
                 stringsAsFactors = FALSE,
                 check.names = FALSE)
    })
  
    output$sample_plot <- renderPlot({
      # In this example the tidying is done as part of plotting
      # because the t-test uses the original data format
      sample_dat()  %>%
        gather(population, measurement, -rowid) %>%
      ggplot(aes(x = population, y = measurement, fill = population)) +
        geom_boxplot() +
        geom_point(position = position_jitterdodge(0.5)) +
        theme(axis.text = element_text(size = rel(2)),
              axis.title = element_text(size = rel(2)),
              legend.text = element_text(size = rel(1.5)),
              legend.title = element_text(size = rel(1.5)))
    })
    
    output$sample_result <- renderText({
      # As in the plot, the reactive object must be called
      # with parentheses (because it's actually a function)
      p <- t.test(sample_dat()[["thicc bois"]], 
             sample_dat()[["chonkers"]], 
             paired = FALSE)[["p.value"]]
      # Add an asterisk to "significant" results
      if(p >= 0.05){
        paste("p-value = ", p)
      } else {
        paste("p-value = ", p, "*")
      }
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

