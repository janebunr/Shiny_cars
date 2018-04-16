
library("shiny")
library("ggraph"); library("igraph"); library("RColorBrewer")
library("NLP")
library("XML")
library("SnowballC")  
library("tm")
library("wordcloud")
library("memoise")
library("ggplot2")
library("NLP")
library("wordcloud")
library("memoise")
require("reshape2")
library("reshape2")
library("readxl")
library("quanteda")
library("topicmodels")
library("dplyr")
library("slam")
library("tm")
library("tidytext")
library("tidyr")
library("RColorBrewer")

brands <- list("Tesla"="tesla","Porsche"="porsche","Audi"="audi","BMW"="bmw","Mercedes-Benz"="mercedes")

gettexts <- memoise(function(brand) {
  if (!(brand %in% brands))
    stop("Unknown brand")
  text <- readLines(sprintf("./%s.txt", brand),
                    encoding="UTF-8")
})




getmostcomm <- memoise(function(brand) {
  if (!(brand %in% brands))
    stop("Unknown brand")
  TEXT <- readLines(sprintf("./%s.txt", brand),
                    encoding="UTF-8")
  text_df <- data_frame(text=as.character(TEXT))
  tidy_text <- text_df %>% unnest_tokens(word,text)
  tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0)
    })



get_negated_text <- memoise(function(brand) {
  if (!(brand %in% brands))
    stop("Unknown brand")
  TEXT <- readLines(sprintf("./%s.txt", brand),
                    encoding="UTF-8")
  text_df <- data_frame(text=as.character(TEXT))
  car_bigrams <- text_df %>% unnest_tokens(bigram, text, token="ngrams",n=2)
  bigrams_separated <- car_bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  bigrams_filtered <- bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)
  bigrams_separated %>%
    filter(word1 == "not") %>%
    count(word1, word2, sort = TRUE)
  AFINN <- get_sentiments("afinn") })
  
negation_words <- c("not", "no", "never", "without")



getTermMatrix <- memoise(function(brand) {
  if (!(brand %in% brands))
    stop("Unknown brand")
  text <- readLines(sprintf("./%s.txt", brand),
                    encoding="UTF-8")
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  m = as.matrix(myDTM)
  sort(rowSums(m), decreasing = TRUE)
})



gettopic <- memoise(function(brand) {
  if (!(brand %in% brands))
    stop("Unknown brand")
  TEXT <- readLines(sprintf("./%s.txt", brand),
                    encoding="UTF-8")
  myCorpus <- Corpus(VectorSource(TEXT))
  myCorpus <- tm_map(myCorpus,content_transformer(tolower))
  myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
  myCorpus <- tm_map(myCorpus, removePunctuation)
  myCorpus <- tm_map(myCorpus, removeNumbers)
  myCorpus <- tm_map(myCorpus, removeWords, c(stopwords("english"), 
                                              c( "can", "say", "one"," way", "use", "also","howev","tell","will","much","need","take","tend","even","like",
                                                 "particular","rather","said","get","well","make","ask","come","end", "first", "two","help","often", "may","might", "see",
                                                 "someth", "thing","point","post","look","right","now","think","'ve","'re","anoth","put","set","new","good","want","sure","kind",
                                                 "larg","yes","day","etc","quit","sinc","attempt","lack","seen","awar","littl","ever","moreov","though","found","abl","enough","far","earli","away","achiev",
                                                 "draw","last","never","brief","bit","entir","brief","great","lot","dont","just","bmw","porsche","tesla","audi","mercedes","benz","mercedesbenz","car")))
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  myCorpus <- tm_map(myCorpus, stemDocument)
  dtm <- DocumentTermMatrix(myCorpus,
                            control = list(removePunctuation = TRUE,
                                           stopwords = TRUE, removeNumbers=TRUE, stemming=TRUE,wordLengths = c(4, Inf)))   #### the minWordLength didn't work
  rowTotals <- apply(dtm , 1, sum)
  dtm.new   <- dtm[rowTotals> 0, ] 
})

SEED <- 1234


getbigram <- memoise(function(brand) {
  if (!(brand %in% brands))
    stop("Unknown brand")
  TEXT <- readLines(sprintf("./%s.txt", brand),
                    encoding="UTF-8")
  text_df <- data_frame(text=as.character(TEXT))
  text_bigram <- text_df %>%
    unnest_tokens(bigram,text,token="ngrams",n=2)
  text_bigram %>%
    count(bigram,sort=TRUE)
  bigrams_separated <- text_bigram %>%
    separate(bigram, c("word1","word2"), sep=" ")
  bigrams_filtered <- bigrams_separated %>%
    dplyr::filter(!word1 %in% stop_words$word) %>%
    dplyr::filter(!word2 %in% stop_words$word)
  bigram_counts <- bigrams_filtered %>%
    count(word1,word2, sort=TRUE) %>%
    unite(bigram, word1, word2, sep= " ")
})
  
  getTermMatrix <- memoise(function(brand) {
    if (!(brand %in% brands))
      stop("Unknown brand")
    text <- readLines(sprintf("./%s.txt", brand),
                      encoding="UTF-8")
    myCorpus = Corpus(VectorSource(text))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords,
                      c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
    myDTM = TermDocumentMatrix(myCorpus,
                               control = list(minWordLength = 1))
    m = as.matrix(myDTM)
    sort(rowSums(m), decreasing = TRUE)
  })




ui <- fixedPage(
  titlePanel("Exploring Tweets"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selection", "Choose a brand:",
                  choices = brands),
      actionButton("update", "Change")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Word Clouds",sliderInput("freq","Minimum Frequency:",
                                                             min = 1,  max = 100, value = 15),
                 sliderInput("max",
                             "Maximum Number of Words:",
                             min = 1,  max = 500,  value = 100),plotOutput("plot")),
        tabPanel("Bigram",
                 h4("Consecutive Sequences of Words"),
                 sliderInput("numterms","Number of Terms Listed:",
                                min=1, max=40, value=10), 
                 numericInput("num_filter", "Minimum Number of Co-occurrence of Bigrams", value=200), 
                 tableOutput("ngram"), plotOutput("graphbigram")),
        tabPanel("Topic Model", sliderInput("numtopic","Number of topic",
                                                              min=1, max=30, value=5),
                 sliderInput("termlist","Number of terms listed",min=1, max=50, value=10), 
                 helpText("The terms that are most common within each topic"),plotOutput("topicmodel_output")),
        tabPanel("Locate Keywords-in-context", textInput("keyword","Enter Keywords here",value="sedan"),
                        helpText("Note: Keywords can be one word or more. Try 'self driving' "),
                 sliderInput("numtextaround","Number of context words around keywords",min=1,max=10,value = 2),tableOutput("context")),
        tabPanel("Sentiment Analysis", sliderInput("maxwordcompare","Maximum number of words to compare",min=50,max=300,value = 100),helpText("Most common positive and negative words"),plotOutput("sentiment")),
        tabPanel("Common words and negations", 
                 sliderInput("num_negated_texts","Number of words listed",min=1,max=50,value = 5),
                 helpText("The most common positive or negative words to follow negations such as ‘never’, ‘no’, ‘not’, and ‘without’"),
                 plotOutput("negatedtexts"))
      )
    )
  )
)


server <- function(input, output, session) {
  
  terms <- reactive({
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  comm_pos_neg <- reactive({
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getmostcomm(input$selection)
      })
    })
  })
  
  contexts <- reactive({
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        gettexts(input$selection)
      })
    })
  })
  
  wordcloud_rep <- repeatable(wordcloud)
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  
  kwic_rep <- repeatable(kwic)
  output$context <- renderTable({
    keyword_context <- contexts()
    CONTEXT_DF <- kwic_rep(keyword_context,input$keyword,window=input$numtextaround)
    CONTEXT_DF$docname <- NULL
    CONTEXT_DF$position <- NULL
    CONTEXT_DF
  })

  output$sentiment <- renderPlot({
    comm_sentiment <- comm_pos_neg()
    comparison.cloud(comm_sentiment, colors = c("red2", "blue3"),
                     max.words = input$maxwordcompare)
  })
  
  
corpus_topic <- reactive({
  input$update
  isolate({
    withProgress({
      setProgress(message = "Processing corpus...")
      gettopic(input$selection)
    })
  })
})



topic_corpus <- reactive({
  LDA_rep <- repeatable(LDA)
  ap_lda <- LDA_rep(corpus_topic(), k= input$numtopic, control = list(seed = SEED))
  ap_topics <- tidy(ap_lda, matrix = "beta")
  ap_top_terms <- ap_topics %>%
    group_by(topic) %>%
    top_n(input$termlist, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  ap_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
})



bigram_count <- reactive({
  input$update
  isolate({
    withProgress({
      setProgress(message="Processing corpus...")
      getbigram(input$selection) }) }) })

bigram <- reactive({
  bigram_bigram <- bigram_count()
  bigram_bigram[1:input$numterms,]
})

output$ngram <- renderTable({ 
  input$update
  isolate({
    bigram()
}) })

graph_bigrams <- reactive({
  graph_gram <- bigram_count()
  graphngram <- graph_gram %>%
    filter(n > input$num_filter) %>%
    graph_from_data_frame()
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  ggraph(graphngram, layout = "fr") +
    geom_edge_link(aes(edge_alpha = 5000), show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
})


output$graphbigram <- renderPlot({
  input$update
  isolate({ 
  graph_bigrams()
}) })


count_negated_text <- reactive({
  input$update
  isolate({
    withProgress({
      setProgress(message="Processing corpus...")
      get_negated_text(input$selection) }) }) })


negated_texts <- reactive({
  count_negatedtexts <- count_negated_text()
  count_negatedtexts %>% 
    mutate(contribution = n * score,
           word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
    group_by(word1) %>%
    top_n(input$num_negated_texts, abs(contribution)) %>%
    ggplot(aes(word2, contribution, fill = n * score > 0)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ word1, scales = "free",ncol=1) +
    scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
    xlab("Words preceded by negation term") +
    ylab("Sentiment score * # of occurrences") +
    coord_flip()
})
?facet_wrap

output$negatedtexts <- renderPlot({
  input$update
  isolate({ 
    negated_texts()
  }) })







output$topicmodel_output <- renderPlot({
  input$update
  isolate({ 
    topic_corpus() })
})






}

shinyApp(ui=ui,server=server)