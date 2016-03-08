library(dplyr)
library(magrittr)
library(lubridate)
library(tm)
library(ggplot2)
library(LaF)

#' Create a term document matrix from a text file
#' 
#' @param file.in file to read
#' @return Term Document Matrix for \code{file.in}
#' @examples
#' get_corpus_tdm("myfile.txt")
get_corpus_tdm <- function(file.in, line.count, sampled) {
  file.dat <- get_file(file.in, line.count, sampled)
  corpus.dat <- Corpus(VectorSource(file.dat))
  
  # TASKS
  # 1. tokenize words by eliminating standard stop words,
  #    punctionation, and numbers
  #
  # 2. remove profane words
  #
  # 3. sample lines randomly
  #
  
  stopwords <- c(stopwords(), 
                 read.table("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt", 
                            stringsAsFactors = FALSE)$V1)
  control.list <- list(stopwords = stopwords,
                       removePunctuation = TRUE,
                       removeNumbers = TRUE,
                       tolower = TRUE)
  return(TermDocumentMatrix(corpus.dat, control.list))
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
    corpus.tdm <- get_corpus_tdm(paste0(path.en, "/", f), line.count, sampled)
    corpus.matrix <- as.matrix(corpus.tdm)
    corpus.wc <- rowSums(corpus.matrix)
    corpus.df <- data.frame(cbind(names(corpus.wc),
                                  as.numeric(corpus.wc),
                                  stringsAsFactors = FALSE))
    names(corpus.df) <- c("word", "freq")
    corpus.df$freq <- as.numeric(corpus.df$freq)
    corpus.occurrence <- sapply(1:nrow(corpus.matrix),
                                function(x) {
                                  length(which(corpus.matrix[x, ] > 0)) / ncol(corpus.matrix)
                                })
    corpus.density <- corpus.df$freq / sum(corpus.df$freq)
    
    corpus.df <- transform(corpus.df, density = corpus.density,
                           occurrence = corpus.occurrence)
    corpus.df$fn <- f
    
    if(is.null(corpus)) {
      corpus <- corpus.df
    } else {
      corpus <- rbind(corpus, corpus.df)
    }
  }
  
  if(!is.null(rdata.out)) {
    assign("corpus", corpus, envir = .GlobalEnv)
    save.image(rdata.out)
  }
  
  head(corpus[with(corpus, order(-occurrence)), ])  
}

#readCorpus_main()