
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Novel-Gazing"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
         fileInput("grexport", "Choose CSV File",
                   accept = c("text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         tableOutput("rawdata")
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$rawdata <- renderTable({
     # input$file1 will be NULL initially. After the user selects
     # and uploads a file, it will be a data frame with 'name',
     # 'size', 'type', and 'datapath' columns. The 'datapath'
     # column will contain the local filenames where the data can
     # be found.
     inFile <- input$grexport
     
     if (is.null(inFile)) return(NULL)
     
     x <- read.csv(inFile$datapath, header = TRUE)
     print(head(x))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

