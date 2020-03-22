
library(shiny)
library(shinythemes)
library(tidyverse)
library(stringr)
library(rvest) # for cool html stuff
library(lubridate)
library(curl) #for id'ing agent to server
library(snakecase)
library(tidytext) #for sentiment analysis
library(vegan) # for ecology tab
library(kableExtra)


source(here::here('R','cleanup_csv.R'))
source(here::here('R','basic_diagnostics.R'))
source(here::here('R','scrape_data.R'))
source(here::here('R','sentiments.R'))
source(here::here('R','jumbotron2.R'))
source(here::here('R','genre_diversity.R'))
# Aesthetics --------------------------------------------------------------
bookpal <- c('#ff7506', #dark orange
             '#00eaff', #(cyan)
             '#ffdf06', #(dark yellow)
             '#ffee7d', #(light yellow)
             '#ff2673', #(magenta)
             '#7df4ff') #(light cyan)

# Functions ---------------------------------------------------------------



# ui ----------------------------------------------------------------------
ui <- navbarPage('Novel-gazing',
                 theme = shinytheme("yeti"),
                 tabPanel("Home",
                          fluidPage(column(9, 
                                           br(),
                                           br(),
                                           br(),
                                           jumbotron2("Welcome to Novel-gazing", "This app takes your reading data from Goodreads and shows you a bunch of fun diagnostics. It is a labor of love.",button=FALSE)),
                                    column(1,
                                           imageOutput('preImageLarge',inline = TRUE))
                          )),
                 tabPanel("1. Your books",
                          # Sidebar with a slider input for number of bins 
                          sidebarLayout(
                             sidebarPanel(
                                imageOutput('preImage',inline = TRUE),
                                h4('Upload your Goodreads data.'),
                                fileInput("grexport", "Choose CSV File",
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv"))
                             ),
                             mainPanel(
                                h3('How to get your data'),
                                p('Goodreads allows you to export a .csv file from your account.'),
                                imageOutput('HowToAll',inline = TRUE),
                                h4('Getting your data from Goodreads:'),
                                p('1. From your home page, go to My Books'),
                                p('2. On the left, go to Import/Export'),
                                p('3. Click Export Library to download your reading data as a csv.'),
                                p('4. Upload it to the left, and if you see the header below, you are set to jet!'),
                                br(),
                                br(),
                                h4('A glimpse of your data'),
                                tableOutput('rawdata')
                             ))),
                 tabPanel("2. The big picture",
                          h3('Reading history'),
                          plotOutput('basicstuff'),
                          h3('How long does it take you to read a book after adding it to your shelf?'),
                          fluidRow(column(8,
                                          plotOutput('timeplot')),
                                   column(4,
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                          htmlOutput('timestatement'))
                          )
                 ),
                 tabPanel("3. Webscrape your books"
                          ,
                          sidebarLayout(
                             sidebarPanel(
                                h4('Enter your Goodreads user info'),
                                p('Enter your goodreads user id and username. You can find them in the URL for your Goodreads account. Your account must be public to do this part.'),
                                textInput(inputId = "userid",label = "User ID",placeholder = "Enter your user ID"),
                                textInput(inputId = "username",label = "User name",placeholder = "Enter your user name"),
                                p('NOTE: This will take a LONG time (~3 mins per page) so go get a coffee after you enter your info and click the button.'),
                                actionButton("go", "Get my data!", icon("book", lib = "glyphicon","fa-2x"),
                                             style="color: #fff; background-color: #ff7506; border-color: #ff7506")
                             ),
                             mainPanel(
                                h4('A glimpse of the scraped data'),
                                tableOutput('scraped'),
                                p('Now you can look at some more fun patterns in the descriptions of your books.'),
                                br(),
                                h4('Sentiment analysis of your bookshelf'),
                                plotOutput('afinn'),
                                fluidRow(column(6,
                                                h5('Bing sentiment scores from your book descriptions'),
                                                plotOutput('sentiments')),
                                         column(6,
                                                h5('The top 10 most common words in your book descriptions'),
                                                tableOutput('keywords'))
                                )
                             ))),
                 
                 tabPanel("4. The ecology of your bookshelf",
                          h3('How diverse is your reading?'),
                          p('Once you have successfully webscraped your data, you can look at it like we would an ecological community. How diverse are the genres you read? How has the richness of your reading changed over time?'),
                          fluidRow(column(5,
                                          plotOutput('rainbow_yr')),
                                   column(7,
                                          plotOutput('rainbow_month'))),
                          fluidRow(column(6,
                                          plotOutput('diversity')),
                                   column(6,
                                          plotOutput('rarefaction')))
                 )
)



# Server ------------------------------------------------------------------
server <- function(input, output) {
   # Image
   output$preImage <- renderImage({
      filename <- normalizePath(here::here('NovelGazingApp','images','book_logo.png'))
      list(src = filename,
           alt = 'test',
           width = 240,
           height = 240)
   }, deleteFile = FALSE)
   
   output$preImageLarge <- renderImage({
      filename <- normalizePath(here::here('NovelGazingApp','images','book_logo_long.png'))
      list(src = filename,
           alt = 'test',
           width = 200)
   }, deleteFile = FALSE)
   
   output$HowToAll <- renderImage({
      filename <- normalizePath(here::here('NovelGazingApp','images','HowToAll.png'))
      list(src = filename,
           alt = 'ht1',
           width = 600)
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
   
   scraped_data <- eventReactive(input$go,{
      # Re-run when button is clicked
      #usernumber <- '8200244'
      #username <- 'megsie'
      usernumber <- input$userid
      username <- input$username
      startUrl <- paste('https://www.goodreads.com/review/list/',usernumber,'-',username,sep='')
      #startUrl <- "https://www.goodreads.com/review/list/8200244-megsie"
      #print(startUrl)
      withProgress(message = 'Scraping your reading data', value = 0, {
         
         n = ceiling(getnbooks(stUrl=startUrl)/30) 
         print(startUrl)
         print(n)
         n=1 #for testing!!!
         goodreads <- map_df(1:n, ~{
            Sys.sleep(6) # don't timeout the goodreads server
            #cat(.x)
            incProgress(1/n, detail = paste("Extracting page", .x))
            get_books(.x,stUrl = startUrl)
         })
      })
      goodreads
   })
   
   output$scraped <- renderTable({
      goodreads <- scraped_data()
      (head(goodreads[,1:2]))
   })
   
   output$timeplot <- renderPlot({
      if(is.null(rawdatafile())){NULL}else{
         c <- cleanup_csv(rawdatafile())
         time_to_finish_shelves(cleaned_csv = c,pal = bookpal)$ind_books_plot
      }
   })
   
   output$timestatement <- renderUI({
      if(is.null(rawdatafile())){NULL}else{
         c <- cleanup_csv(rawdatafile())
         yrs <- time_to_finish_shelves(cleaned_csv = c,pal = bookpal)$nyears_to_finish
         textout <- paste('At your current rate, it will take you','<b>', yrs,'</b>','years to finish everything on your shelves.')
         HTML(paste(textout))
      }
   })
   
   
   # Scraped data and sentiments
   output$sentiments <- renderPlot({
      x <- get_cmatrix_and_genres(scraped_data = scraped_data())$goodreads_read
      tidied <- tidy_the_books(read_books_data = x)
      get_bing_plot(tidied_books = tidied,pal = bookpal)
   })
   
   output$afinn <- renderPlot({
      x <- get_cmatrix_and_genres(scraped_data = scraped_data())$goodreads_read
      tidied <- tidy_the_books(read_books_data = x)
      get_AFINN_plot(tidied_books = tidied)
   })
   
   output$keywords <- function(){ 
      x <- get_cmatrix_and_genres(scraped_data = scraped_data())$goodreads_read
      tidied <- tidy_the_books(read_books_data = x)
      df <- get_word_table(tidied_books = tidied) %>%
         rename('Year read'=year_read,
                'Word'=word,
                'Number of times this word appears in your read shelf'=n) %>%
         as.data.frame() 
      df <- df[1:10,]
      
      kable(df) %>%
         kable_styling("striped", full_width = F) 
   }
   
   output$rainbow_yr <- renderPlot({
      get_rainbow_plot(scraped_data = scraped_data(),pal = bookpal,whichplot = 'yearly')
   })
   
   output$rainbow_month <- renderPlot({
      get_rainbow_plot(scraped_data = scraped_data(),pal = bookpal,whichplot = 'monthly')
   })
   
   output$diversity <- renderPlot({
      x <- get_cmatrix_and_genres(scraped_data = scraped_data())$community
      get_divplot(community = x)
   })
   
   output$rarefaction <- renderPlot({
      x <- get_cmatrix_and_genres(scraped_data = scraped_data())$community
      get_rarefaction(community = x,pal = bookpal)
   })
}



# Run the application 
shinyApp(ui = ui, server = server)

