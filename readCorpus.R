library(dplyr)
library(magrittr)
library(lubridate)
library(tm)
library(ggplot2)

#' Create a term document matrix from a text file
#' 
#' @param file.in file to read
#' @return Term Document Matrix for \code{file.in}
#' @examples
#' get_corpus_tdm("myfile.txt")
get_corpus_tdm <- function(file.in, line.count) {
  file.dat <- get_file(file.in, line.count)
  corpus.dat <- Corpus(VectorSource(file.dat))
  control.list <- list(stopwords = TRUE,
                       removePunctuation = TRUE,
                       removeNumbers = TRUE,
                       tolower = TRUE)
  return(TermDocumentMatrix(corpus.dat, control.list))
}

#' Read in a text file
#' 
#' @param path file to read
#' @return text contents of \code{path}
#' @examples
#' get_file("myfile.txt")
get_file <- function(path, line.count = -1) {
  con <- file(path, open = "rt")
  if (line.count == -1) {
    text <- readLines(con)
  } else {
    text <- readLines(con, n = line.count)
  }
  close(con)
  return(text)
}

#' Do the work
#' @param line.count read only first \code{line.count} lines of each file
#' @param rdata.out file to save memory environment
#' @return Nothing returned
#' @examples
#' readCorpus_main(debug = TRUE)
readCorpus_main <- function(line.count = 100, rdata.out = "output.RData") {
  path.en <- "final/en_US"
  files.en <- dir(path = path.en, pattern = "en_.+\\.txt")
  
  corpus <- NULL
  for(f in files.en) {
    
    # adapted from Machine Learning for Hackers, Drew Conway & John Myles White
    # Chapter 3, p.80 
    corpus.tdm <- get_corpus_tdm(paste0(path.en, "/", f), line.count)
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
    save.image(rdata.out)
    assign("corpus", corpus, envir = .GlobalEnv)
  }
  
  head(corpus[with(corpus, order(-occurrence)), ])  
}

#readCorpus_main()