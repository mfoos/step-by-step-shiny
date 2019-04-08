library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

ui <- fluidPage(
  
  fluidRow(
    column(width = 12,
           actionButton("do_sample", "Moar!"),
           plotOutput("sample_plot"),
           textOutput("sample_result")
    )
  )
)

server <- function(input, output) {
  
    sample_count <- reactiveVal(10)
    
    observeEvent(input$do_sample,{
      x <- sample_count() + 10
      sample_count(x)
    })
   
    sample_dat <- reactive({
      tmp <- sample_count()
      data.frame("rowid" = paste0("row", seq(1, tmp)),
                 "thicc bois" = rnorm(tmp, 0, 1),
                 "chonkers" = rnorm(tmp, 0.2, 1),
                 stringsAsFactors = FALSE,
                 check.names = FALSE)
    })

    output$sample_plot <- renderPlot({
      sample_dat()  %>%
        gather(population, measurement, -rowid) %>%
      ggplot(aes(x = population, y = measurement, fill = population)) +
        geom_boxplot() +
        geom_point(position = position_jitterdodge(0.5)) +
        ggtitle(paste0("You sampled ", nrow(sample_dat()), " absolute units")) +
        theme(axis.text = element_text(size = rel(2)),
              axis.title = element_text(size = rel(2)),
              legend.text = element_text(size = rel(1.5)),
              legend.title = element_text(size = rel(1.5)),
              plot.title = element_text(size = rel(2)))
    })

    output$sample_result <- renderText({
      p <- t.test(sample_dat()[["thicc bois"]],
             sample_dat()[["chonkers"]],
             paired = FALSE)[["p.value"]]
      if(p >= 0.05){
        paste("p-value = ", p)
      } else {
        paste("p-value = ", p, "*")
      }
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

