
library(shiny)
library(shinythemes)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- navbarPage('Novel-Gazing',
                 theme = shinytheme("yeti"),
                 tabPanel("1. Your books",
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         imageOutput('preImage'),
         h4('Upload your Goodreads data.'),
         p('Goodreads allows you to export your bookshelves as a CSV file.'),
         fileInput("grexport", "Choose CSV File",
                   accept = c("text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv"))
      ),
      mainPanel(
         h3('Your reading at a glance'),
         h4('Check your data'),
         tableOutput('rawdata'),
         
         
      ))),
   tabPanel("2. Reading history"),
   tabPanel("3. Diversity & sentiments")
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
   # Image
   output$preImage <- renderImage({
      filename <- normalizePath(here::here('NovelGazingApp','images','test.jpg'))
      # Return a list containing the filename and alt text
      list(src = filename,
           alt = 'test')
   }, deleteFile = FALSE)
   
   # Raw data
   output$rawdata <- renderTable({
     inFile <- input$grexport
     if (is.null(inFile)) return(NULL)
     x <- read.csv(inFile$datapath, header = TRUE)
     show <- x %>% 
              select(Title, Author, ISBN, My.Rating,Number.of.Pages)
     head(show,n = 3)
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

