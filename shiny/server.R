library(shiny)
library(ggplot2)
source("readCorpus.R")

if (FALSE) { # not ready for production
  blogs <- get_files("../final/en_US/", "en_US.blogs.txt", line.count = 200, use.cache = FALSE)
  readCorpus_main(blogs, global.out = "blogs", rdata.out = "blogs")
  
  news <- get_files("../final/en_US/", "en_US.news.txt", line.count = 200, use.cache = FALSE)
  readCorpus_main(news, global.out = "news")
  
  tweets <- get_files("../final/en_US/", "en_US.twitter.txt", line.count = 200, use.cache = FALSE)
  readCorpus_main(tweets, global.out = "tweets")
} else {
  load("cached.RData")
}

shinyServer(function(input, output, session) {

  # search box
  
  output$query <- renderText({
    input$query
  })
  
  default_query <- reactive({
    switch(input$corpus,
           "blogs" = blogs.df$word[1],
           "news" = news.df$word[1],
           "tweets" = news.df$word[1]
    )
  })
  
  #observe({
  #  updateTextInput(session, "query",
  #                  label = "Type query here:",
  #                  value = default_query()
  #  )
  #})
  
  # frequency table
  corpusInput.df <- reactive({
    switch(input$corpus,
           "blogs" = blogs.df %>% 
             filter(ngram >= input$num_ngrams) %>%
             mutate(source = "blogs"),
           "news" = news.df %>%
             filter(ngram >= input$num_ngrams) %>%
             mutate(source = "news"),
           "tweets" = tweets.df %>% 
             filter(ngram >= input$num_ngrams) %>%
             mutate(source = "tweets")
           )
  })
  
  output$view <- renderTable({
    df <- corpusInput.df()
    head(df %>%
           filter(ngram >= input$num_ngrams) %>%
           arrange(desc(freq)), 
         n = input$num_results)
  })

  # corpus preview
  
  output$summary <- renderPrint({
    corpus <- corpusInput()
    head(corpus, n = 3)
  })
  
  corpusInput <- reactive({
    switch(input$corpus,
           "blogs" = blogs[grep(input$query, blogs)],
           "news" = news[grep(input$query, news)],
           "tweets" = tweets[grep(input$query, tweets)]
           )
  })
  
  # ngram histogram 
  output$main_plot <- reactivePlot(function() {
    
    df <- corpusInput.df()
    ggplot(df,
           aes(x = freq,
               fill = ngram)) + 
      geom_histogram(bins = 10) +
      scale_x_log10() + 
      ggtitle(df$source[1])
  })
  
})