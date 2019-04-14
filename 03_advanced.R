library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RSQLite)

# Pull gene names from the database when they occur in both the
# gene list and the expression table
conn <- dbConnect(RSQLite::SQLite(), "GSE60424.db")
choosenames <- dbGetQuery(conn, "SELECT symbol
                          FROM identifiers i
                          INNER JOIN expr e
                          ON i.ensId = e.genenames;")
choosenames <- choosenames[[1]]
dbDisconnect(conn)

ui <- fluidPage(
  
  titlePanel("Immune Cell Gene Expression Data"),
  
  sidebarLayout(
    sidebarPanel(
      # Create input widget, but do not populate it with any genes
      selectizeInput("ingene", "Choose a gene:", choices = NULL, selected = NULL),
      
      helpText(h4("Data source:")),
      helpText("Linsley PS, Speake C, Whalen E, Chaussabel D.", em("Copy number loss of the interferon gene cluster in                     melanomas is linked to reduced T cell infiltrate and poor patient prognosis."), 
               "PLoS One 2014 Oct 14;9(10):e109760."),
      a(href = "https://www.ncbi.nlm.nih.gov/pubmed/25314013", "On PubMed"), br(),
      a(href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE60424", "On GEO")
    ),  
    mainPanel(
      plotOutput("theplot"),
      helpText("Expression values were normalized using the TMM (trimmed mean of m-value) method.",
               "This means that counts have been normalized by library size but not gene length.", 
               "Plot shows Tukey boxplots: whiskers extend to the highest/lowest value within the ",
               "box limit +/- 1.5 * IQR. Samples beyond that range are plotted as points.")
    )
  )
)

server <- function(input, output, session) {
  
  # Populate the selection widget after the rest of the page loads.
  # Using server = TRUE makes it even faster
  updateSelectizeInput(session, "ingene", choices = choosenames, selected = "HBB", server = TRUE)
  
  # Function to pull selected gene from SQLite database rather
  # than reading in a full text ("flat") file
  makeTinyframe <- function(gene, dbpath){
    conn <- dbConnect(RSQLite::SQLite(), dbpath)
    meta <- dbGetQuery(conn, "SELECT * FROM meta;")
    # This text is SQL code:
    exprquery <- sprintf("SELECT i.symbol, e.*
                         FROM identifiers i
                         JOIN expr e
                         ON i.ensId = e.genenames
                         WHERE i.symbol == %s;", shQuote(gene))
    expr <- dbGetQuery(conn, exprquery)
    dbDisconnect(conn)
    
    rownames(expr) <- expr$genenames
    togene <- expr[,grep("lib", colnames(expr), invert = TRUE)]
    expr <- expr[,grep("lib", colnames(expr))]
    expr <- t(expr)
    
    tinyframe <- merge(meta, expr, by.x = "library", by.y = 0)
    tinyframe <- tinyframe %>% 
      gather(gene, TPM, starts_with("ENS")) %>%
      merge(., togene, by.x = "gene", by.y = "genenames") %>%
      rename(genename = symbol)
    return(tinyframe)
    
  }
  
  # Function to create the plot
  makePlot <- function(tinyframe){
    
    # This is just because the whole dataframe is passed to the function,
    # but we want the gene name itself for the title text
    printgene <- unique(tinyframe$genename)
    
    ggplot(tinyframe, aes(x = genename, y = TPM, color = celltype)) + 
      ggtitle(paste0(printgene, " expression by cell type across conditions")) +
      geom_boxplot() +
      facet_grid(~ disease_status) +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
  }
  
  inputgene <- reactive({
    # This allows us to return custom text if there is something wrong
    # with the input. The app will not continue until the "need" is met.
    validate(
      need(input$ingene != "", "Please select a gene")
    )
    input$ingene
  })
  
  output$theplot <- renderPlot({
    # Call the querying and plotting functions
    tf <- makeTinyframe(inputgene(), "GSE60424.db")
    makePlot(tf)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

