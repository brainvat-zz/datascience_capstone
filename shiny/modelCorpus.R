setwd("~/Dropbox/Data Science/Capstone - July 2015/07-MAR-2016/shiny")

library(tm)
library(tau)
library(filehash)
library(tools)

# Limits for creating ngram frequency tables
MEM_LIMIT <- 100000 # rows per table

# realized from research and feedback I was going in
# completely the wrong direction with the TermDocumentMatrix
# approach

# http://www.cs.columbia.edu/~kathy/NLP/ClassSlides/Class3-ngrams09/ngrams.pdf
# https://class.coursera.org/nlp/lecture/14

# instead, need to calculate unigram, bigram, and trigram counts
# then use simple Markov chain assumptions to calculate the probability of
# the next word in the sentence from the probabilities of the frequencies
# of the previous bigram or trigram.  Beginning and end of sentences are
# important, so keep punctuation. Keep stop words, profanity, and sparse
# words in this model because you need them for accuracy.

#' Do the work
#' @param export If TRUE, publish corpus by-products to Global Environment
#' @param remove.options List of options for cleaning the corpus
#' @param ngram.names takes a list to specify how many ngram tables to build
#' @param seed.value overrride random seeding for test / train
#' @return Nothing returned
#' @examples
#' modelCorpus_main()

modelCorpus_main <- function(export = TRUE, 
                             remove.options = list(profanity = FALSE,
                                                   stopwords = TRUE,
                                                   numbers = TRUE,
                                                   punctuation = FALSE,
                                                   case = TRUE,
                                                   spaces = TRUE),
                             ngram.names = c("unigrams", 
                                             "bigrams", 
                                             "trigrams", 
                                             "quadgrams"),
                             seed.value = 8675309) {
  
  if (file.exists("modelCache.RData")) {
    
    log("Loading cached data...")
    load("modelCache.RData")
    
  } else {
    
    ######
    # Read and pre-process corpus
    ##########
    
    log("Reading tweets corpus")
    tweets <- VCorpus(DirSource(pattern = "en_US.twitter.txt",
                                recursive = TRUE, 
                                encoding = "UTF-8"))
    
    log("Reading news corpus")
    news <- VCorpus(DirSource(pattern = "en_US.news.txt",
                              recursive = TRUE, 
                              encoding = "UTF-8"))
    
    log("Reading blogs corpus")
    blogs <- VCorpus(DirSource(pattern = "en_US.blogs.txt",
                               recursive = TRUE,
                               encoding = "UTF-8"))
    
    # downcase all text, strip out whitespace, numbers & punctuation
    # remove stop words and profanity
    profanity <- read.table("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt", 
                            stringsAsFactors = FALSE)$V1
    
    # RECOMMENDED: strip case
    if (remove.options$case) {
      log("Downcasing corpus")
      tweets <- tm_map(tweets, content_transformer(tolower))
      news <- tm_map(news, content_transformer(tolower))
      blogs <- tm_map(blogs, content_transformer(tolower))
    }
    
    # RECOMMENDED: remove numbers
    if (remove.options$numbers) {
      log("Stripping numbers from corpus")
      tweets <- tm_map(tweets, removeNumbers)
      news <- tm_map(news, removeNumbers)
      blogs <- tm_map(blogs, removeNumbers)
    }
    
    # NOT RECOMMENDED: remove punctuation
    # You need to know where sentences begin and end
    # and it's not easy to tell the difference between the end
    # of a sentence and contractions like Mr. or Mrs.
    # If you remove all punctuation before creating the
    # ngram probabilities, you could end up with a much larger
    # matrix since every paragraph becomes one large run-on
    # sentence
    if (remove.options$punctuation) {
      log("Stripping punctuation from corpus")
      tweets <- tm_map(tweets, removePunctuation)
      news <- tm_map(news, removePunctuation)
      blogs <- tm_map(blogs, removePunctuation)
    }
    
    # CHALLENGING: remove stop words
    # Stop words need to be predicted too.  But, the
    # challenge is, if you leave stop words in, then
    # you may get a lot less accurate results because these
    # words appear very commonly in sentences.  Can throw
    # off your backwards chaining.
    if (remove.options$stopwords) {
      log("Removing stop words from corpus")
      tweets <- tm_map(tweets, function(x) removeWords(x, stopwords()))
      news <- tm_map(news, function(x) removeWords(x, stopwords()))
      blogs <- tm_map(blogs, function(x) removeWords(x, stopwords()))
    }
    
    # RECOMMENDED: remove profanity
    # As the recent case with Microsoft Tay, you have to be
    # careful leaving profane words in your model.
    # http://www.pcworld.com/article/3048157/data-center-cloud/the-internet-turns-tay-microsofts-millennial-ai-chatbot-into-a-racist-bigot.html
    if (remove.options$profanity) {
      log("Removing profanity from corpus")
      tweets <- tm_map(tweets, function(x) removeWords(x, profanity))
      news <- tm_map(news, function(x) removeWords(x, profanity))
      blogs <- tm_map(blogs, function(x) removeWords(x, profanity))
    }
    
    # RECOMMENDED: remove whitespace
    # Whitespace doesn't help much with words and some
    # space is created by other scrubbing above
    if (remove.options$spaces) {
      log("Removing excess white space from corpus")
      tweets <- tm_map(tweets, stripWhitespace)
      news <- tm_map(news, stripWhitespace)
      blogs <- tm_map(blogs, stripWhitespace)
    }
    
    export_workspace(with.image = TRUE)
  }
  
  ######
  # Split data for training, testing
  ##########

  set.seed(seed.value) # Jenny
  TRAIN <- 0.75
  TEST <- 0.25
  
  if (seed.value == 8675309) {
    train.dir <- "training"
    test.dir <- "testing"
  } else {
    train.dir <- paste0("training_", seed.value)
    test.dir <- paste0("testing_", seed.value)
  }

  # Clean up past runs
  if (file.exists(paste0(train.dir, ".db"))) {
    log("Removing old training corpus")
    file.remove(paste0(train.dir, ".db"))
  }
    
  if (!dir.exists(train.dir)) {
    dir.create(train.dir)
  }
  
  if (!dir.exists(test.dir)) {
    dir.create(test.dir)
  }
  
  log("Extracting training data for tweets")
  randomized.tweets <- sample(tweets[[1]][[1]])
  train.tweets <- round(TRAIN * length(randomized.tweets))
  write(randomized.tweets[1:train.tweets], paste0(train.dir, "/tweets.txt"))
  write(randomized.tweets[-(1:train.tweets)], paste0(test.dir, "/tweets.txt"))
  rm(tweets, randomized.tweets, train.tweets)
  
  log("Extracting training data for news")
  randomized.news <- sample(news[[1]][[1]])
  train.news <- round(TRAIN * length(randomized.news))
  write(randomized.news[1:train.news], paste0(train.dir, "/news.txt"))
  write(randomized.news[-(1:train.news)], paste0(test.dir, "/news.txt"))
  rm(news, randomized.news, train.news)
  
  log("Extracting training data for blogs")
  randomized.blogs <- sample(blogs[[1]][[1]])
  train.blogs <- round(TRAIN * length(randomized.blogs))
  write(randomized.blogs[1:train.blogs], paste0(train.dir, "/blogs.txt"))
  write(randomized.blogs[-(1:train.blogs)], paste0(test.dir, "/blogs.txt"))
  rm(blogs, randomized.blogs, train.blogs)
  
  ######
  # Read training corpus and create ngram frequency table
  ##########
  
  training <-PCorpus(DirSource(train.dir, 
                               encoding = "UTF-8",
                               mode="text"),
                     dbControl = list(dbName = paste0(train.dir, ".db"), 
                                      dbType = "DB1"))
  
  for (j in 1:length(ngram.names)) {
    for (i in 1:length(training)) {
      this.corp <- file_path_sans_ext(training[[i]][[2]]$id)
      this.gram <-  ngram.names[j]
      
      max.length <- min(MEM_LIMIT, length(training[[i]][[1]]))
      log(paste0("Building ", this.gram, " for ", 
                 prettyNum(max.length, big.mark = ","), 
                 " rows of ", this.corp))
      
      ngrams <- textcnt(c(training[[i]][[1]][1:max.length]),
                        # split = "[[:space:][:digit:]]+", # keep punctuation
                        method = "string",
                        n = j,
                        decreasing = TRUE)
      assign(paste0(this.corp, ".", this.gram), ngrams)
    } 
  }
  rm(ngrams, i, j)
  
  if (export) {
    export_workspace()
  }
}

export_workspace <- function(with.image = FALSE) {
  if (with.image) {
    if (!file.exists("modelCache.RData")) {
      log("Saving processed corpus to disk image for faster loading next time")
      save(tweets, 
           news, 
           blogs, 
           file = "modelCache.RData",
           envir = parent.frame())
    }
  } else {
    for (k in ls(pattern = "grams", envir = parent.frame())) {
      assign(k, get(k, envir = parent.frame()), envir = .GlobalEnv)
    }
  }
}

log <- function(t) {
  print(t)
}


