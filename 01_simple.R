library(shiny)


ui <- fluidPage( 
  
  fluidRow( # Determines that this app will use the "fluid page" (grid) layout method
    column(width = 12,
           # Creates the input widget
           textInput("textinputlabel", "What's up?"),
           # Designates the place-holder for the output generated
           textOutput("newtext")
           
    )
  )
)

server <- function(input, output) {
   
    # Transforms the input to uppercase and writes it where it can be read
    # into the user interface
    output$newtext <- renderText({
      toupper(input$textinputlabel)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

