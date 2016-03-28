setwd("~/Dropbox/Data Science/Capstone - July 2015/07-MAR-2016/shiny")
options(scipen=999)

library(tm)
library(tau)
#library(filehash)
library(tools)
library(openNLP)
library(stringi)
library(dplyr)

# Limits for creating ngram frequency tables
MEM_LIMIT <- 1000 # rows per table
SENTENCE_START <- "ZZZSTARTZZZ"
SENTENCE_END <- "ZZZENDZZZ"
NON_SENTENCE <- paste0(SENTENCE_END, " ", SENTENCE_START)
TRAIN_SAMPLE_SIZE <- 0.60
LOG_LEVEL <- 1

# realized from research and feedback I was going in
# completely the wrong direction with the TermDocumentMatrix
# approach

# instead, need to calculate unigram, bigram, and trigram counts
# then use simple Markov chain assumptions to calculate the probability of
# the next word in the sentence from the probabilities of the frequencies
# of the previous bigram or trigram.  Beginning and end of sentences are
# important, so keep punctuation. Keep stop words, profanity, and sparse
# words in this model because you need them for accuracy.

#' Do the work
#' @param export If TRUE, publish corpus by-products to Global Environment
#' @param remove.options List of options for cleaning the corpus
#' @param ngram.names takes a list of ngram tables to build
#' @param corpus.names takes a list of corpus sources to use
#' @param seed.value overrride random seeding for test / train
#' @param mem.limit maximum number of corpus lines processed per source
#' @return Nothing returned
#' @examples
#' modelCorpus_main()

modelCorpus_main <- function(export = TRUE, 
                             remove.options = list(profanity = FALSE,
                                                   stopwords = FALSE,
                                                   numbers = TRUE,
                                                   punctuation = TRUE,
                                                   case = TRUE,
                                                   spaces = TRUE,
                                                   sentences = TRUE),
                             ngram.names = c("unigrams", 
                                             "bigrams", 
                                             "trigrams", 
                                             "quadgrams",
                                             "pentagrams"),
                             corpus.names = c("blogs",
                                              "twitter",
                                              "news"),
                             seed.value = 8675309,
                             mem.limit = MEM_LIMIT) {
  
  if (file.exists("modelCache.RData")) {
    
    modelCorpus_log("Loading cached data...")
    load("modelCache.RData")
    
  } else {
    
    ######
    # Read and pre-process corpus
    ##########
    
    for (this.corpus.name in corpus.names) {
      modelCorpus_log("Reading corpus ", 
                      this.corpus.name, 
                      " MEM LIMIT = ", 
                      prettyNum(mem.limit, big.mark = ","))
      this.corpus <- VCorpus(DirSource(pattern = paste0("en_US.", this.corpus.name, ".txt"),
                                       recursive = TRUE, 
                                       encoding = "UTF-8"))
      
      # downcase all text, strip out whitespace, numbers & punctuation
      # remove stop words and profanity
      profanity <- read.table("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt", 
                              stringsAsFactors = FALSE)$V1
      
      # RECOMMENDED: strip case
      if (remove.options$case) {
        modelCorpus_log("Downcasing corpus ", this.corpus.name)
        this.corpus <- tm_map(this.corpus, content_transformer(tolower))
      }
      
      # RECOMMENDED: remove numbers
      if (remove.options$numbers) {
        modelCorpus_log("Stripping numbers from corpus ", this.corpus.name)
        this.corpus <- tm_map(this.corpus, removeNumbers)
      }
      
      # RECOMMENDED: mark sentence boundaries
      # Adapted from Stack Overflow example
      # http://stackoverflow.com/questions/18712878/r-break-corpus-into-sentences
      
      # MUST come after remove numbers function above!
      
      convert_text_to_marked_sentences <- function(text, lang = "en") {
        
        # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector employing the default model for language 'en'. 
        sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
        
        # Convert text to class String from package NLP
        
        f <- function(t) {
          t <- as.String(t)
          
          # Sentence boundaries in text
          sentence.boundaries <- annotate(t, sentence_token_annotator)
          
          # Extract sentences
          sentences <- t[sentence.boundaries]
          modelCorpus_log("Found ", length(sentences), " sentences in this line", log.level = 2)
          
          # return marked sentences
          return(as.String(sapply(sentences, 
                                  function(x) paste0(SENTENCE_START,
                                                     " ", 
                                                     gsub("[\r\n]", "", x), 
                                                     " ", 
                                                     SENTENCE_END))))        
        }
        
        max.rows <- min(mem.limit, length(text))
        modelCorpus_log("Converting ", max.rows, " rows of corpus text to marked sentences")
        
        return(sapply(text[1:max.rows], f))
      }
      
      ct.fun <- content_transformer(function(x) convert_text_to_marked_sentences(x))
      
      if (remove.options$sentences) {
        modelCorpus_log("Marking sentence boundaries in corpus ", this.corpus.name)
        this.corpus <- tm_map(this.corpus, ct.fun)
      }    
      
      # CHALLENGING: remove punctuation
      # You need to know where sentences begin and end
      # and it's not easy to tell the difference between the end
      # of a sentence and contractions like Mr. or Mrs.
      # If you remove all punctuation before creating the
      # ngram probabilities, you could end up with a much larger
      # matrix since every paragraph becomes one large run-on
      # sentence
      
      # MUST come after sentence function above!
      
      if (remove.options$punctuation) {
        modelCorpus_log("Stripping punctuation from corpus ", this.corpus.name)
        this.corpus <- tm_map(this.corpus, removePunctuation)
      }
      
      # CHALLENGING: remove stop words
      # Stop words need to be predicted too.  But, the
      # challenge is, if you leave stop words in, then
      # you may get a lot less accurate results because these
      # words appear very commonly in sentences.  Can throw
      # off your backwards chaining.
      if (remove.options$stopwords) {
        modelCorpus_log("Removing stop words from corpus ", this.corpus.name)
        this.corpus <- tm_map(this.corpus, function(x) removeWords(x, stopwords()))
      }
      
      # RECOMMENDED: remove profanity
      # As the recent case with Microsoft Tay, you have to be
      # careful leaving profane words in your model.
      # http://www.pcworld.com/article/3048157/data-center-cloud/the-internet-turns-tay-microsofts-millennial-ai-chatbot-into-a-racist-bigot.html
      if (remove.options$profanity) {
        modelCorpus_log("Removing profanity from corpus ", this.corpus.name)
        this.corpus <- tm_map(this.corpus, function(x) removeWords(x, profanity))
      }
      
      # RECOMMENDED: remove whitespace
      # Whitespace doesn't help much with words and some
      # space is created by other scrubbing above
      if (remove.options$spaces) {
        modelCorpus_log("Removing excess white space from corpus ", this.corpus.name)
        this.corpus <- tm_map(this.corpus, stripWhitespace)
      }
      
      modelCorpus_log("Exporting processed corpus ", this.corpus.name)
      assign(this.corpus.name, this.corpus)
    }
  }
  rm(this.corpus, this.corpus.name)
  
  ######
  # Split data for training, testing
  ##########

  set.seed(seed.value) # Jenny
  TRAIN <- TRAIN_SAMPLE_SIZE
  TEST <- 1 - TRAIN
  
  if (seed.value == 8675309) {
    train.dir <- "training"
    test.dir <- "testing"
  } else {
    train.dir <- paste0("training_", seed.value)
    test.dir <- paste0("testing_", seed.value)
  }

  # Clean up past runs
  if (file.exists(paste0(train.dir, ".db"))) {
    modelCorpus_log("Removing old training corpus")
    file.remove(paste0(train.dir, ".db"))
  }
    
  if (!dir.exists(train.dir)) {
    dir.create(train.dir)
  }
  
  if (!dir.exists(test.dir)) {
    dir.create(test.dir)
  }
  
  for (this.corpus.name in corpus.names) {
    modelCorpus_log("Extracting training data for ", this.corpus.name)
    this.vcorpus <- get(this.corpus.name)
    this.corpus <- this.vcorpus[[1]][[1]]
    randomized.this.corpus <- sample(this.corpus)
    train.this.corpus <- min(mem.limit, round(TRAIN * length(randomized.this.corpus)))
    write(randomized.this.corpus[1:train.this.corpus], paste0(train.dir, "/", this.corpus.name, ".txt"))
    write(randomized.this.corpus[-(1:train.this.corpus)], paste0(test.dir, "/", this.corpus.name, ".txt"))
  }
  rm(this.vcorpus, this.corpus.name, this.corpus, 
     randomized.this.corpus, train.this.corpus)
  
  ######
  # Read training corpus and create ngram frequency table
  ##########
  
  training <-PCorpus(DirSource(train.dir, 
                               encoding = "UTF-8",
                               mode="text"),
                     dbControl = list(dbName = paste0(train.dir, ".db"), 
                                      dbType = "DB1"))
  
  for (i in 1:length(training)) {
    this.corp <- file_path_sans_ext(training[[i]][[2]]$id)
    max.length <- length(training[[i]][[1]]) # already constrained by mem.limit
    
    for (j in 1:length(ngram.names)) {
      this.gram <-  ngram.names[j]
      modelCorpus_log("Building ", this.gram, " for ", prettyNum(max.length, big.mark = ","),
                      " rows of ", this.corp)
      
      ngrams <- textcnt(c(training[[i]][[1]][1:max.length]),
                        # split = "[[:space:][:digit:]]+", # keep punctuation
                        method = "string",
                        n = j,
                        decreasing = TRUE)
      
      # convert to data frame
      ngrams <- data.frame(word = names(ngrams), count = as.vector(ngrams),
                           stringsAsFactors = FALSE)
      
      # do not treat ngrams at sentence boundaries as special
      ngrams <- ngrams[!grepl(pattern = paste0(SENTENCE_START, "|", SENTENCE_END), 
                              x = ngrams$word, 
                              ignore.case = TRUE), ]

      # get rid of non-sense ngrams
      # if (j > 1) {
      #   ngrams <- ngrams[!grepl(pattern = NON_SENTENCE, 
      #                           x = ngrams$word, 
      #                           ignore.case = TRUE), ]
      # }
      # ngrams <- ngrams[ngrams$word != tolower(SENTENCE_START), ]
      # ngrams <- ngrams[ngrams$word != tolower(SENTENCE_END), ]
      
      assign(paste0(this.corp, ".", this.gram), ngrams)
    } 
    
    # calculate maximum likelihood estimate
    # https://class.coursera.org/nlp/lecture/14
    # https://class.coursera.org/nlp/lecture/128
    # http://www.cs.columbia.edu/~kathy/NLP/ClassSlides/Class3-ngrams09/ngrams.pdf
    # http://www.cs-114.org/wp-content/uploads/2015/01/NgramModels.pdf

    rm(ngrams)
    all.ngrams <- NULL
    for (j in 1:length(ngram.names)) {
      if (j > 1) {
        this.gram <- paste0(this.corp, ".", ngram.names[j])
        previous.gram <- paste0(this.corp, ".", ngram.names[j-1])
        
        assign("ngrams", get(this.gram))
        assign("ngrams.previous", get(previous.gram))
        
        ngrams.count <- nrow(ngrams)
        ngrams.previous.count <- nrow(ngrams.previous)
        modelCorpus_log("Calculating MLE for ", ngrams.previous.count, " in ", previous.gram)
        ngrams.previous$NPlus1Ct <- 
          sapply(1:ngrams.previous.count, 
                 FUN = function(x) {
                    this.word <- paste0(ngrams.previous[x, ]$word, " ")
                    nplus1ct <- sum(ngrams[stri_startswith_fixed(ngrams$word, 
                                                                 this.word), ]$count)
                    return(nplus1ct)
                   })
        assign(previous.gram, ngrams.previous)
        
        # update master ngram tables
        modelCorpus_log("Updating master ngrams table with ", 
                        prettyNum(nrow(ngrams.previous), big.mark = ","),
                        " rows of ", this.corp)
        
        ngrams.previous$ngram <- ngram.names[j-1]
        if (is.null(all.ngrams)) {
          all.ngrams <- ngrams.previous
        } else {
          all.ngrams <- rbind(all.ngrams, ngrams.previous) %>%
            group_by(ngram, word) %>%
            summarize(count = sum(count),
                      NPlus1Ct = sum(NPlus1Ct))
        }
      }
    }
  }
  rm(ngrams, ngrams.previous, i, j, max.length,
     this.corp, this.gram, previous.gram,
     ngrams.count, ngrams.previous.count)
    
  if (export) {
    export_workspace(with.image = TRUE)
  }
}

export_workspace <- function(with.image = FALSE) {
  # export to global environment
  for (k in ls(pattern = "grams", envir = parent.frame())) {
    assign(k, get(k, envir = parent.frame()), envir = .GlobalEnv)
  }

  if (with.image) {
    modelCorpus_log("Saving current environment data frames: ", 
                    paste0(ls(pattern = "grams", 
                              envir = parent.frame()), 
                           collapse = ", "),
                    log.level = 2)
    save(list = ls(pattern = "grams", envir = parent.frame()), 
         file = "modelCache.RData",
         envir = parent.frame())
    
    save.image("modelCorpusFinalState.RData")
  }
}

modelCorpus_log <- function(..., log.level = 1) {
  if (log.level <= LOG_LEVEL) {
    print(paste0(...))
  }
}

modelCorpus_reset_cache <- function() {
  if (file.exists("modelCache.RData")) {
    file.remove("modelCache.RData")
  }
  
  if (file.exists("modelCorpusFinalState.RData")) {
    file.remove("modelCorpusFinalState.RData")
  }
  
  t <- list.files(path = "testing", pattern = "txt$", full.names = TRUE)
  if (length(t) > 0) {
    do.call(file.remove,as.list(t))
  }
  
  t <- list.files(path = "training", pattern = "txt$", full.names = TRUE)
  if (length(t) > 0) {
    do.call(file.remove,as.list(t))
  }
}

#modelCorpus_reset_cache(); modelCorpus_main()
#modelCorpus_reset_cache(); modelCorpus_main(corpus.names = c("blogs"), mem.limit = 100)
#modelCorpus_reset_cache(); modelCorpus_main(mem.limit = 100)
#modelCorpus_reset_cache(); modelCorpus_main(mem.limit = 1000, ngram.names = c("unigrams", "bigrams", "trigrams"))

