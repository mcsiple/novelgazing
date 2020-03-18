
library(shiny)
library(shinythemes)
library(tidyverse)
library(here)

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
   tabPanel("2. Webscrape your books"
            ,
            sidebarLayout(
               sidebarPanel(
                  h4('Enter your account data'),
                  p('Enter your goodreads user id and username. You can find them in the URL for your Goodreads account.'),
                  numericInput(inputId = 'userid',value = 0,label = 'User ID'),
                  textInput(inputId = "username",label = "User name",placeholder = "Enter your user name"),
                  p('NOTE: This will take a LONG time (~3 mins per page) so go get a coffee after you enter your info and click the button.'),
                  actionButton("go", "Fetch my data!", icon("book", lib = "glyphicon","fa-2x"),
                               style="color: #fff; background-color: #ff7506; border-color: #ff7506")
                 ),
               mainPanel(verbatimTextOutput("userid"))
             )),
   tabPanel("3. Diversity & sentiments",
            h3('How diverse is your reading?',
               p('Once you have successfully webscraped your data, you can look at it like we would an ecological community. How diverse are the genres you read? ')))
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

