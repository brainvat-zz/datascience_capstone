library(dplyr)
library(magrittr)
library(lubridate)
library(tm)
library(ggplot2)
library(LaF)
library(RWeka)
library(SnowballC)

# WEEK 1 - TASKS
# 1-1. tokenize words by eliminating standard stop words,
#    punctionation, and numbers
#
# 1-2. remove profane words
#
# 1-3. sample lines randomly
#

# WEEK 2 - TASKS
#
# 2-1. calculate 1-, 2-, and 3-word frequencies in the data
#
# 2-2. find minimum number of unique words needed to cover 50%
#    of the corpus and 90% of the corpus
# 
# 2-3. remove foreign words from the term document matrix
#
# 2-4. explore ways to increase coverage 
#

#' Create a Corpus from a text file
#' 
#' @param file.in file to read
#' @return Corpus for \code{file.in}
#' @examples
#' get_corpus("myfile.txt")
get_corpus_dat <- function(file.in, line.count, sampled) {
  file.dat <- get_file(file.in, line.count, sampled)
  return(Corpus(VectorSource(file.dat)))
}

#' Create a Term Document Matrix from a Corpus
#' 
#' @param corpus.dat Corpus object
#' @return term document matrix for \code{file.in}
#' @examples
#' get_corpus_tdm("myfile.txt")
get_corpus_tdm <- function(corpus.dat) {
  
  # Stack Overflow fix
  # http://stackoverflow.com/questions/26834576/big-text-corpus-breaks-tm-map
  corpus.dat <- tm_map(corpus.dat,
                       content_transformer(function(x) iconv(x, to = 'utf-8-mac', sub = '')),
                       mc.cores = 1)  
  
  # Stack Overflow fix
  # http://stackoverflow.com/questions/24771165/r-project-no-applicable-method-for-meta-applied-to-an-object-of-class-charact
  corpus.dat <- tm_map(corpus.dat, content_transformer(tolower), mc.cores = 1)
  corpus.dat <- tm_map(corpus.dat, PlainTextDocument, mc.cores = 1)

  # task 1-1
  corpus.dat <- tm_map(corpus.dat, removePunctuation, mc.cores = 1)
  
  # task 1-2
  stopwords <- c(stopwords(), 
                 read.table("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt", 
                            stringsAsFactors = FALSE)$V1)
  corpus.dat <- tm_map(corpus.dat, function(x) removeWords(x, stopwords), mc.cores = 1)
  
  corpus.dat <- tm_map(corpus.dat, removeNumbers, mc.cores = 1)
  corpus.dat <- tm_map(corpus.dat, stripWhitespace, mc.cores = 1)
  
  # task 2-3
  corpus.dat <- tm_map(corpus.dat, stemDocument, language = "english", mc.cores = 1)
  
  # Stack Overflow fix
  # http://stackoverflow.com/questions/18504559/twitter-data-analysis-error-in-term-document-matrix
  #corpus.dat <- tm_map(corpus.dat, wordStem, language = "english", mc.cores = 1)
  
  # task 2-1
  # Adapted from "Word, Pair and Triplet Frequencies" by Jadyen MacRae
  # http://jaydenmacrae.blogspot.com/2013/12/word-pair-and-triplet-frequencies.html
  ngt <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))
  corpus.tdm <- TermDocumentMatrix(corpus.dat, control = list(tokenize = ngt))
  
  return(corpus.tdm)
}

#' Read in a text file
#' 
#' @param path file to read
#' @param line.count number of lines to read
#' @param sampled Boolean to indicate if lines are read randomly
#' @return text contents of \code{path}
#' @examples
#' get_file("myfile.txt")
#' get_file("myfile.txt", line.count = 1000, sampled = TRUE)
get_file <- function(path, line.count = -1, sampled = TRUE) {
  con <- file(path, open = "rt")
  if (line.count == -1) {
    text <- readLines(con)
  } else {
    # task 1-3
    if (sampled) {
      close(con)
      return(sample_lines(path, line.count))
    } else {
      text <- readLines(con, n = line.count)
    }
  }
  close(con)
  return(text)
}

#' Do the work
#' @param line.count read only first \code{line.count} lines of each file
#' @param sampled BOOLEAN indicates whether to randomize lines read in corpus
#' @param rdata.out file to save memory environment
#' @return Nothing returned
#' @examples
#' readCorpus_main(debug = TRUE)
readCorpus_main <- function(line.count = 100, sampled = TRUE, rdata.out = "output.RData") {
  path.en <- "final/en_US"
  files.en <- dir(path = path.en, pattern = "en_.+\\.txt")
  
  corpus <- NULL
  for(f in files.en) {
    
    # adapted from Machine Learning for Hackers, Drew Conway & John Myles White
    # Chapter 3, p.80 
    corpus.dat <- get_corpus_dat(paste0(path.en, "/", f), line.count, sampled)
    corpus.tdm <- get_corpus_tdm(corpus.dat)
    corpus.matrix <- as.matrix(corpus.tdm)
    corpus.wc <- rowSums(corpus.matrix)
    corpus.df <- data.frame(cbind(names(corpus.wc),
                                  as.numeric(corpus.wc)),
                            stringsAsFactors = FALSE)
    names(corpus.df) <- c("word", "freq")
    corpus.df$freq <- as.numeric(corpus.df$freq)
    corpus.df$ngram <- sapply(gregexpr("\\S+", corpus.df[, c("word")]), length)
    corpus.df$fn <- f
    
    
    if(is.null(corpus)) {
      corpus <- corpus.df
    } else {
      corpus <- rbind(corpus, corpus.df)
    }
  }
  corpus <- corpus[complete.cases(corpus), ]
  
  if(!is.null(rdata.out)) {
    assign("corpus", corpus, envir = .GlobalEnv)
    save.image(rdata.out)
  }
}

readCorpus_main()