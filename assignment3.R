#
# Author: Oisin McCaffrey
# Title: Assignment3 - MA5111 - Microarray analysis - 16307771
# Date: 23 October 2020
# 
#
#    
#

library(shiny)

ui <- fluidPage(
  
  # Title
  titlePanel("MA5111 Assignment 4"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),
    
    
    # Number of Data displayed
    
    numericInput(inputId = "genes",
                 label = "Number of Genes:",
                 value = 10),
    
    # Slider input for adj p-value 
    h2("Plot Threshold:"),
    sliderInput(inputId = "AdjPValue", label = "Adj. P-value",
                min = 0.0000, max = 0.1, value = 0.05),
    
    #  Include one other feature of your choice!
    
    selectInput("colour", "Change the colour of your Volcano Plot!",
                choices = c("lightsalmon1", "powderblue", "yellow", "blue", "darkorchid2")),
    
    # Slider input for Log2FC 
    h2("log2FC"),
    sliderInput(inputId = "Log2FC", label = "Log2FC",
                min = 0.0001, max = 8, value = 1)),
  
  mainPanel(
    tableOutput("tab"),
    plotOutput("plot")
  )
 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Reactive function to load data
  LoadData <- reactive({
    data <- input$file1
    read.csv(data$datapath, header = T, sep = "\t")
  })
  
  # Table
  
  output$tab <- renderTable({
    data <- input$file1 # access elements within this using data. Data just contains info about the file, not the file itself i.e. havent read it in here
    if(is.null(data)) # dont throw an error initially - data will be null initially
      return(NULL)
    
    head(read.csv(data$datapath, header = T, sep="\t"), n = input$genes) # datapath - datapath element of temp object location of data object, change sep as necessary
  })
  
  # Volcano Plot
  
  output$plot <- renderPlot({
    Gene <- LoadData()
    
    # Prevent warning
    if (is.null(Gene)) {
      return(NULL)
    }
    
    #Significant subset
    Sig <- Gene[(Gene$adj.P.Val <= input$AdjPValue & abs(Gene$logFC) >= input$Log2FC), ]
    
    # Title of Plot
    Title <- c(paste("Volcano Plot", "Adj P-value = ", as.character(input$AdjPValue), "Log2FC = ", as.character(input$log2FC)), paste("DEG:", as.character(nrow(Sig))))
    
    # Volcano Plot 
    plot(Gene$logFC, -log10(Gene$adj.P.Val),
         pch="*", xlab ="Log2 Fold Change",
         ylab="-10log (adjusted p-value)",
         main = Title)
    abline(h=-log10(input$AdjPValue), v=c(-input$Log2FC), col="mediumorchid1", lty=3)
    points(Sig$logFC, -log10(Sig$adj.P.Val), col=input$colour, pch="*")
  })
}

#Run Application
shinyApp(ui = ui, server = server)



