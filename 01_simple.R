library(shiny)


ui <- fluidPage(
  
  fluidRow(
    column(width = 12,
           textInput("textinputlabel", "What's up?"),
           textOutput("newtext")
           
    )
  )
)

server <- function(input, output) {
   
    output$newtext <- renderText({
      toupper(input$textinputlabel)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

