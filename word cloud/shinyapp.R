library (shiny)
library(tm)
library(wordcloud)
library(RCurl)


ui <- fluidPage(
  titlePanel("Word Cloud"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selection", "Choose a Source:",choices = books),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq", "Minimum Frequency:", min = 1,  max = 50, value = 15),
      sliderInput("max", "Maximum Number of Words:", min = 1,  max = 300,  value = 200)
    ),
    mainPanel( plotOutput("plot") )
  )
)

server <-function(input, output, session) {
  
  books <<- list("Students' Comments" ="summer", "Teachers' Feadbacks" = "merchant", "Romeo and Juliet" = "romeo")
  
  getTermMatrix <- function(book) { 
    print (book)
    text <- book
    myCorpus = Corpus(VectorSource(text))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords,  c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
    myDTM = TermDocumentMatrix(myCorpus,  control = list(minWordLength = 1))
    m = as.matrix(myDTM)
    sort(rowSums(m), decreasing = TRUE)
  }
  terms <- reactive({
    input$update
    isolate({  withProgress({ setProgress(message = "Processing corpus...")
      getTermMatrix(input$selection)  })
    })
  })
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),  min.freq = input$freq, max.words=input$max,  colors=brewer.pal(20, "Dark2"))
  })
}

shinyApp(ui=ui,server = server)