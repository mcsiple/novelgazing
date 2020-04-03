# Libraries ---------------------------------------------------------------
library(shiny)
library(shinythemes)
library(tidyverse)
library(stringr)
library(textdata)
library(textcat) #for detecting language
library(ggsidekick)
library(magrittr) # optional webscraping
library(lubridate)
library(curl) #for id'ing agent to server
library(httr)
library(snakecase)
library(reshape2)
library(patchwork)
library(ggrepel)
library(tidytext) #for sentiment analysis
library(vegan) # for ecology measures
library(kableExtra) # for nice tables

# Functions ---------------------------------------------------------------
source(here::here('R','cleanup_csv.R'))
source(here::here('R','basic_diagnostics.R'))
source(here::here('R','sentiments.R'))
source(here::here('R','jumbotron2.R'))
source(here::here('R','genre_diversity.R'))
source(here::here('R','get_genre.R'))
source(here::here('R','get_cmatrix_and_genres.R'))
source(here::here('R','summary_stats.R'))

# Aesthetics --------------------------------------------------------------
bookpal <- c('#ff7506', #dark orange
             '#00eaff', #(cyan)
             '#ffdf06', #(dark yellow)
             '#ffee7d', #(light yellow)
             '#ff2673', #(magenta)
             '#7df4ff') #(light cyan)

# ui ----------------------------------------------------------------------
ui <- navbarPage('Novel-gazing',
                 theme = shinytheme("yeti"),
                 tabPanel("Home",
                          fluidRow(column(9, 
                                           br(),
                                           br(),
                                           br(),
                                           jumbotron2("Welcome to Novel-gazing", "This app takes your reading data from Goodreads and shows you a bunch of fun diagnostics. It is a labor of love.",button=FALSE)),
                                    column(1,
                                           imageOutput('preImageLarge',inline = TRUE)
                                           )
                                   ),
                                    fluidRow(column(12,
                                           h5('Built with',
                                              img(src = 'https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png', height = '30px'),
                                              'in',
                                              img(src = 'https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png', height = '30px'),
                                              'using',
                                              img(src = 'http://svgur.com/i/5t1.svg',height = '40px'),
                                              'and data from',
                                              img(src = 'https://s.gr-assets.com/assets/icons/goodreads_icon_100x100-4a7d81b31d932cfc0be621ee15a14e70.png', height = '30px'),
                                              'Goodreads.'),
                                           h6('By Megsie Siple'),
                                           h6('Illustrations by Ashley Siple')))
                          ),
                 tabPanel("Your books",
                          sidebarLayout(
                             sidebarPanel(
                                imageOutput('books_tower',inline = TRUE),
                                h4('Upload your Goodreads data.'),
                                fileInput("grexport", "Choose CSV File",
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv"))
                             ),
                             mainPanel(
                                h3('How to get your data'),
                                p('Goodreads allows you to export a .csv file from your account. You must use the website, not the phone app, to do this.'),
                                imageOutput('HowToAll',inline = TRUE),
                                br(),
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
                 tabPanel("Your taste in books",
                          h3("Your chart-topping books"),
                          tableOutput('short_long'),
                          fluidRow(column(6,
                                          tableOutput("top3authcount")),
                                   column(6,
                                          tableOutput("top3authrating")
                                          )),
                          h3('How does your taste compare to that of other Goodreads users?'),
                          p('Have you ever wondered how your taste in books compares to that of your peers? The plot below shows your ratings compared to those of other users.'),
                          plotOutput('likeplot')
                          
                          
                 ),
                 tabPanel("The long term",
                          h3('Reading history'),
                          plotOutput('basicstuff'),
                          h3('How long does it take you to read a book after adding it to your shelf?'),
                          fluidRow(column(8,
                                          plotOutput('timeplot')
                                          ),
                                   column(4,
                                          br(),
                                          htmlOutput('timestatement'),
                                          imageOutput('books_sit',inline = TRUE)
                                          # 
                                          )
                                   )
                 ),
                 tabPanel("The ecology of your bookshelf",
                          sidebarLayout(
                             sidebarPanel(
                                imageOutput('books_mice',inline = TRUE),
                                h3('Get some more info on your books'),
                                p('We can use OpenLibrary to get the topics of all the books on your shelves. This takes a while, but it will be worth it!'),
                                p('NOTE: This may take a while so go get a coffee after you click the button.'),
                                actionButton("genrego", "Get genre info!", icon("book", lib = "glyphicon","fa-2x"),
                                             style="color: #fff; background-color: #ff7506; border-color: #ff7506"),
                                br(),
                                br(),
                                imageOutput('books_group',inline = TRUE)
                             ),
                             mainPanel(
                          h3('How has the community on your bookshelf changed over time?'),
                          p('Once all the subjects of your books have been detected, you can look at it like we would an ecological community. How diverse are the subjects you read? Do you read more of some subjects in certain seasons?'),
                          br(),
                          p("This is the most finicky of the tabs because it uses an API to collect subject data on your books. If you get an error, just try again later."),
                          htmlOutput('HTTP_status'),
                          br(),
                          h4('The composition of your read shelf over time'),
                          plotOutput('rainbow_yr'),
                          plotOutput('rainbow_month'),
                          
                          br(),
                          h4('Diversity and subject richness'),
                          p('We can apply some community ecology methods to your bookshelf to ask questions like, how diverse is your reading? The plot on the left shows a common diversity index, Shannon diversity. The higher it is, the more diverse your subject material is. The plot on the right shows a rarefaction curve. If it has plateaued, you have leveled out in terms of topics you tend to read. We use this type of curve in ecology to estimate whether the samples, on the x axis, have adequately characterized the community.'),
                          fluidRow(column(6,
                                          plotOutput('diversity')),
                                   column(6,
                                          plotOutput('rarefaction'))),
                          h3('How similar is your to-read shelf to your read shelf?'),
                          p('In ecology, we compare animal and plant communities from different samples using multivariate statistics. The plot below shows each year of data as a single point, and its position is based on the subjects you read (or wanted to read) that year. Do the yellow and blue points overlap? Then you may be reading the same types of books you aspire to read.'),
                          actionButton("mds_go", "Compare my shelves!", icon("book", lib = "glyphicon","fa-2x"),
                                       style="color: #fff; background-color: #ff7506; border-color: #ff7506"),
                          fluidRow(column(4,
                                          br(),
                                          br(),
                                          br(),
                                          imageOutput('books_lie',inline = TRUE)
                                          #,
                                          #imageOutput('books_hug',inline = TRUE)
                                          ),
                                   column(8,
                                          plotOutput('mdscomp'))),
                          fluidRow(column(12,
                                          h5('A note about the Open Library data'),
                                          p('This app uses Open Library to look up the subjects listed for each book by its ISBN. These data are not available for every book, and some important info might be missing. If you want to webscrape genres from your Goodreads account, you can do that to get a more comprehensive list of genres. The code to do this is in the project repository (see Contributing for the link).')))
                 ))),
                 tabPanel("Contributing",
                          h3("Share your thoughts or contribute code"),
                          p("Do you have ideas for features or want to contribute your own code and/or are a wizard at JSON? I welcome ideas and consider this a work in progress."),
                          tags$ol(tags$li("Privacy: This app does not save any of your data. It is gone as soon as you close the window"),
                             tags$li("This project uses user data from", a(href="https://www.goodreads.com/api","Goodreads")," but this app barely scratches the surface!"),
                             tags$li("If you encounter an issue, or a feature you'd like to request, submit it on the ",a(href="https://github.com/mcsiple/novelgazing", "GitHub repo for this project") ),
                             tags$li("Love the illustrations? Follow", a(href="https://www.instagram.com/badger_friend/","@badger_friend"),"on Instagram")),
                          imageOutput('books_hug')
                          )
)



# server ------------------------------------------------------------------
server <- function(input, output) {
   
   
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
   
   

# Basic all-time book info ------------------------------------------------
   output$short_long <- function(){
      if(is.null(rawdatafile())){NULL}else{
         dat <- cleanup_csv(rawdatafile())
         s <- sltable(cleaned_books = dat)
         kable(s) %>%
            kable_styling(bootstrap_options ="bordered", full_width = F) %>%
            column_spec(1, bold = TRUE)
         
      }}
   
   output$top3authcount <- function(){
      if(is.null(rawdatafile())){NULL}else{
         dat <- cleanup_csv(rawdatafile())
         s <- top3authors(cleaned_books = dat,ranking = 'count')
         kable(s) %>%
            kable_styling(bootstrap_options = "bordered", full_width = F)
      }}
   
   output$top3authrating <- function(){
      if(is.null(rawdatafile())){NULL}else{
         dat <- cleanup_csv(rawdatafile())
         s <- top3authors(cleaned_books = dat,ranking = 'rating')
         kable(s) %>%
            kable_styling(bootstrap_options ="bordered", full_width = F)
      }}
   
   output$likeplot <- renderPlot({
      if(is.null(rawdatafile())){NULL}else{
         plotdat <- cleanup_csv(rawdatafile())
         likeplot(plotdat)}
   })
# Long-term info ----------------------------------------------------------


   output$basicstuff <- renderPlot({
      if(is.null(rawdatafile())){NULL}else{
         plotdat <- cleanup_csv(rawdatafile())
         basic_diagnostics(plotdat,pal = bookpal)}
   })
   
   full_data <- eventReactive(input$genrego,{
      cleaned_books <- cleanup_csv(rawdatafile())
      genrelist <- list()
      n <- nrow(cleaned_books)
      withProgress(message = 'Getting subject data',{
         for(i in 1:nrow(cleaned_books)){
               genrelist[[i]] <- get_genre(cleaned_books = cleaned_books,i = i)
               incProgress(1/n, detail = paste("Extracting subjects for book", i,"of",n))
            }
         }
      ) #end withProgress section
      nf <- tibble(title = cleaned_books$title,
                   author = cleaned_books$author,
                   month_read = cleaned_books$month_read,
                   year_read = cleaned_books$year_read,
                   exclusive_shelf = cleaned_books$exclusive_shelf,
                   book_genres = genrelist)
      nf
      }
   )
   
   comm_genres <- reactive({
      get_cmatrix_and_genres(fulldata = full_data())
      })
   
   output$full <- renderTable({
      goodreads <- full_data()
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
         textout <- paste('At your current rate, it will take you',
                          '<b>',
                          yrs,
                          '</b>',
                          'years to finish everything on your shelves.')
         HTML(paste(textout))
      }
   })

# Ecology tab -------------------------------------------------------------
   output$HTTP_status <- renderUI({
      c <- cleanup_csv(rawdatafile())
      firstrealisbn <- c$isbn[which(c$isbn != '')[1]]
      test <-  httr::GET(paste0('https://openlibrary.org/api/books?bibkeys=ISBN:',firstrealisbn,'&jscmd=data&format=json'))
      if(httr::status_code(test) == 503){
         txout <-  "The OpenLibrary Server is returning a 503 error. Try again later!"}else{
            txout <- ""
         }
         HTML(txout)
   })
   
   output$rainbow_yr <- renderPlot({
      get_rainbow_plot(comm_genre = comm_genres(),
                       pal = bookpal,
                       whichplot = 'yearly')
   })
   
   output$rainbow_month <- renderPlot({
      get_rainbow_plot(comm_genre = comm_genres(),
                       pal = bookpal,
                       whichplot = 'monthly')
   })
   
   output$circles <- renderPlot({
      c <- cleanup_csv(rawdatafile())
      monthfigure(cleaned_csv = c)
   })
   
   output$diversity <- renderPlot({
      x <- comm_genres()$community
      get_divplot(community = x)
   })
   
   output$rarefaction <- renderPlot({
      x <- comm_genres()$community
      get_rarefaction(community = x,pal = bookpal)
   })
   
   output$test <- renderText({
      if(is.null(rawdatafile())){NULL}else{
         plotdat <- cleanup_csv(rawdatafile())
      x <- get_genre(cleaned_books = plotdat,i = 1)
      paste(x)
      }
   })
   
   mds_react <- eventReactive(input$mds_go,{
      x <- get_cmatrix_and_genres(fulldata = full_data())
      y <- x$community
      z <- x$community_toread
      plotout <- get_mds_comp(community = y,community_toread = z,pal = bookpal)
      plotout
   })
   
   output$mdscomp <- renderPlot({
      mds_react()
      # x <- get_cmatrix_and_genres(fulldata = full_data())
      # y <- x$community
      # z <- x$community_toread
      # plotout <- get_mds_comp(community = y,community_toread = z,pal = bookpal)
   })
   

# Images ------------------------------------------------------------------

   output$preImage <- renderImage({
      filename <- normalizePath(here::here('www','book_logo.png'))
      print(filename)
      list(src = filename,
           alt = 'test',
           width = 240,
           height = 240)
   }, deleteFile = FALSE)
   
   output$preImageLarge <- renderImage({
      filename <- normalizePath(here::here('www','book_logo_long.png'))
      print(filename)
      list(src = filename,
           alt = 'test2',
           width = 200)
   }, deleteFile = FALSE)
   
   output$HowToAll <- renderImage({
      filename <- normalizePath(here::here('www','HowToAll.png'))
      print(filename)
      list(src = filename,
           alt = 'ht1',
           width = 600)
   }, deleteFile = FALSE)
   
   
   output$books_tower <- renderImage({
      filename <- normalizePath(here::here('www','books_tower.png'))
      list(src = filename,
           alt = 'tower',
           width = 260,
           height = 260)
   }, deleteFile = FALSE)
   # 
   output$books_mice <- renderImage({
      filename <- normalizePath(here::here('www','books_mice.png'))
      list(src = filename,
           alt = 'mice',
           width = 240)
   }, deleteFile = FALSE)
   # # 
   output$books_sit <- renderImage({
      filename <- normalizePath(here::here('www','books_sit.png'))
      list(src = filename,
           alt = 'sit',
           width = 240,
           height = 240)
   }, deleteFile = FALSE)
   
   output$books_lie <- renderImage({
      filename <- normalizePath(here::here('www','books_lie.png'))
      list(src = filename,
           alt = 'sit',
           width = 240,
           height = 240)
   }, deleteFile = FALSE)
   
   output$books_hug <- renderImage({
      filename <- normalizePath(here::here('www','books_hug.png'))
      list(src = filename,
           alt = 'hug',
           width = 240,
           height = 240)
   }, deleteFile = FALSE)
   
   output$books_group <- renderImage({
      filename <- normalizePath(here::here('www','books_group.png'))
      list(src = filename,
           alt = 'sit',
           width = 240,
           height = 240)
   }, deleteFile = FALSE)
} # end server

# Run the application 
shinyApp(ui = ui, server = server)

