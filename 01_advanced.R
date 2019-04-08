library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

expr_table <- read.table("expression_matrix_w_metadata.txt", sep = ",", header = TRUE, stringsAsFactors = FALSE)
metacols <- grep("^ENSG", colnames(expr_table), invert = TRUE, value = TRUE)

allgenes <- read.table("mart_export.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
allgenes <- allgenes[allgenes$ensId %in% colnames(expr_table),]
lookuptable <- unstack(allgenes)

ui <- fluidPage(
  
  titlePanel("Immune Cell Gene Expression Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("ingene", "Choose a gene:", choices = sort(allgenes$symbol), selected = "HBB"),
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
        req(input$ingene)
      
        ens <- lookuptable[[input$ingene]]
        gene <- input$ingene
        
        tinyframe <- expr_table[,c(ens, metacols)]
        tinyframe <- tinyframe %>% gather(variable, value, one_of(ens))
        p <- ggplot(tinyframe, aes(x = variable, y = value, color = cellType)) + 
          ggtitle(paste0(gene, " expression by cell type across conditions")) +
          geom_boxplot() +
          ylab("Normalized Counts") +
          theme(axis.text.x = element_blank(), 
                axis.ticks.x = element_blank(), 
                axis.title.x = element_blank())
        
        if (length(ens) > 1) {
          p <- p + facet_grid(variable ~ diseaseStatus, scales = "free_y")
        } else { 
          p <- p + facet_grid(~ diseaseStatus)
        }
        return(p)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

