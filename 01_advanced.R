library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read in the expression table, and identify the non-gene columns (metadata)
expr_table <- read.table("expression_matrix_w_metadata.txt", sep = ",", header = TRUE, stringsAsFactors = FALSE)
metacols <- grep("^ENSG", colnames(expr_table), invert = TRUE, value = TRUE)

# Read in the list of genes, discard genes with no expression data,
# and turn the gene name/gene id table into a lookup list
allgenes <- read.table("mart_export.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
allgenes <- allgenes[allgenes$ensId %in% colnames(expr_table),]
lookuptable <- unstack(allgenes)

ui <- fluidPage(
  
  titlePanel("Immune Cell Gene Expression Data"),
  
  sidebarLayout( # Determines that this app will use the sidebar layout method
    sidebarPanel(
      # Populate the dropdown from the allgenes vector values
      selectizeInput("ingene", "Choose a gene:", choices = sort(allgenes$symbol), selected = "HBB"),
      # Don't be that guy who doesn't give others credit
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

server <- function(input, output) {
   
    output$theplot <- renderPlot({
        # Check that the input gene is not null before plotting
        req(input$ingene)
      
        # Take the human-friend user input (gene name) and convert
        # it to Ensembl Id
        ens <- lookuptable[[input$ingene]]
        gene <- input$ingene
        
        # Pull out the chosen gene and metadata only
        tinyframe <- expr_table[,c(ens, metacols)]
        
        # Tidy the dataframe for plotting
        tinyframe <- tinyframe %>% 
          gather(variable, value, one_of(ens))
        
        ggplot(tinyframe, aes(x = variable, y = value, color = cellType)) + 
          ggtitle(paste0(gene, " expression by cell type across conditions")) +
          geom_boxplot() +
          facet_grid(~ diseaseStatus) +
          ylab("Normalized Counts") +
          theme(axis.text.x = element_blank(), 
                axis.ticks.x = element_blank(), 
                axis.title.x = element_blank())
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

