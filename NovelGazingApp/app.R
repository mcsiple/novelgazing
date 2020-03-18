
library(shiny)
library(shinythemes)
library(tidyverse)

source(here('R','cleanup_csv.R'))
source(here('R','basic_diagnostics.R'))


# Aesthetics --------------------------------------------------------------
bookpal <- c('#ff7506', #dark orange
             '#00eaff', #(cyan)
             '#ffdf06', #(dark yellow)
             '#ffee7d', #(light yellow)
             '#ff2673', #(magenta )
             '#7df4ff') #(light cyan )



# ui ----------------------------------------------------------------------
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
         h4('A glimpse of your data'),
         tableOutput('rawdata'),
         h4('Reading history'),
         plotOutput('basicstuff')
         
      ))),
   tabPanel("2. Reading history"),
   tabPanel("3. Diversity & sentiments")
   )



# Server ------------------------------------------------------------------
server <- function(input, output) {
   # Image
   output$preImage <- renderImage({
      filename <- normalizePath(here::here('NovelGazingApp','images','book_logo.png'))
      # Return a list containing the filename and alt text
      list(src = filename,
           alt = 'test',
           width = 240,
           height = 240)
   }, deleteFile = FALSE)
   
   # Raw data
   rawdatafile <- reactive({
      inFile <- input$grexport
      if (is.null(inFile)) return(NULL)
      x <- read.csv(inFile$datapath, header = TRUE)
      x
   })
   
   output$rawdata <- renderTable({
   if (is.null(rawdatafile())) {NULL}else{
     x <- rawdatafile()
     show <- x %>% 
              select(Title, Author, ISBN, My.Rating,Number.of.Pages)
     head(show,n = 3)
   }
   })
   
   # Basic diagnostic plots
   output$basicstuff <- renderPlot({
      if(is.null(rawdatafile())){NULL}else{
      plotdat <- cleanup_csv(rawdatafile())
      basic_diagnostics(plotdat,pal = bookpal)}
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

