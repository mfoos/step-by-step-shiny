library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

ui <- fluidPage(
  
  fluidRow(
    column(width = 12,
           # Create input widget and place-hold outputs
           sliderInput("sample_count", "How many times should we sample each population?",
                       value = 10,
                       min = 1,
                       max = 500),
           plotOutput("sample_plot"),
           br(),
           textOutput("max")
    )
  )
)

server <- function(input, output) {
  
  # Initialize the reactive value at 0.
  # The reactive value can store intermediate values
  # while keeping them in "reactive context"
  sample_max <- reactiveVal(0)
  
  # When the sample data changes, it
  # triggers the contents of the "observe" expression
  # to run. The observe does not return a value like "render"
  # expressions do.
  observe({
    new_max <- max(sample_dat()$measurement)
    sample_max(max(new_max, sample_max()))
  })
  
  sample_dat <- reactive({
    # Sample from two normal distributions with slightly different means
    # The rowid is just a unique identifier to allow tidying
    data.frame("rowid" = paste0("row", seq(1,input[["sample_count"]])),
               "thicc bois" = rnorm(input$sample_count, 0, 1),
               "chonkers" = rnorm(input$sample_count, 0.2, 1),
               stringsAsFactors = FALSE,
               check.names = FALSE) %>%
      # "Tidy" the dataframe into "long" format for plotting
      gather(population, measurement, -rowid)
  })
  
  output$sample_plot <- renderPlot({
    # Plot the two populations
    ggplot(sample_dat(), aes(x = population, y = measurement, fill = population)) +
      geom_boxplot() +
      geom_point(position = position_jitterdodge(0.5)) +
      theme(axis.text = element_text(size = rel(2)),
            axis.title = element_text(size = rel(2)),
            legend.text = element_text(size = rel(1.5)),
            legend.title = element_text(size = rel(1.5)))
  })
  
  output$max <- renderText({
    paste0("All-session maximum measurement: ", sample_max())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

