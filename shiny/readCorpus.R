suppressMessages(library(dplyr))
#library(magrittr)
#library(lubridate)
suppressMessages(library(tm))
#library(ggplot2)
suppressMessages(library(LaF))
suppressMessages(library(RWeka))
suppressMessages(library(SnowballC))
suppressMessages(library(textcat))
#library(ff)
#library(ffbase)
#library(biglm)
#library(bigmemory)

# Globals
cache.corpus.dat <- ".corpus.dat"
cache.corpus.tdm <- ".corpus.tdm"
cache.text.dat <- ".text.dat"

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
#' @param line.count max number of lines to read
#' @param sampled randomly choose lines from file
#' @param use.cache fetch previously loaded corpus from disk
#' @return Corpus for \code{file.in}
#' @examples
#' get_corpus("myfile.txt")
get_corpus_dat <- function(text.dat, use.cache = TRUE) {
  if (use.cache & file.exists(cache.corpus.dat)) {
    load(cache.corpus.dat, envir = environment())
  } else {
    corpus.dat <- Corpus(VectorSource(text.dat))
    if (use.cache & !file.exists(cache.corpus.dat)) {
      save(corpus.dat, file = cache.corpus.dat)
    }
  }
  return(corpus.dat)
}

#' Create a Term Document Matrix from a Corpus
#' 
#' @param corpus.dat Corpus object
#' @param use.cache Fetch previously created TDM from disk
#' @return term document matrix for \code{file.in}
#' @examples
#' get_corpus_tdm("myfile.txt")
get_corpus_tdm <- function(corpus.dat, use.cache = TRUE, use.stemming = FALSE) {
  if (use.cache & file.exists(cache.corpus.tdm)) {
    load(cache.corpus.tdm, envir = environment())
  } else {
    # Stack Overflow fix
    # http://stackoverflow.com/questions/26834576/big-text-corpus-breaks-tm-map
    # this doesn't seem to really fix anything
    corpus.dat <- tm_map(corpus.dat,
                         content_transformer(function(x) iconv(x, to = 'utf-8-mac', sub = '')),
                         mc.cores = 1,
                         mc.silent = TRUE)  
    
    # Stack Overflow fix
    # http://stackoverflow.com/questions/24771165/r-project-no-applicable-method-for-meta-applied-to-an-object-of-class-charact
    corpus.dat <- tm_map(corpus.dat, content_transformer(tolower), mc.cores = 1, mc.silent = TRUE)
    corpus.dat <- tm_map(corpus.dat, PlainTextDocument, mc.cores = 1, mc.silent = TRUE)
    
    # task 1-1
    corpus.dat <- tm_map(corpus.dat, removePunctuation, mc.cores = 1, mc.silent = TRUE)
    
    # task 1-2
    stopwords <- c(stopwords(), 
                   read.table("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt", 
                              stringsAsFactors = FALSE)$V1)
    corpus.dat <- tm_map(corpus.dat, function(x) removeWords(x, stopwords), mc.cores = 1, mc.silent = TRUE)
    
    corpus.dat <- tm_map(corpus.dat, removeNumbers, mc.cores = 1, mc.silent = TRUE)
    corpus.dat <- tm_map(corpus.dat, stripWhitespace, mc.cores = 1, mc.silent = TRUE)
    
    # task 2-3
    if (use.stemming) {
      corpus.dat <- tm_map(corpus.dat, stemDocument, language = "english", mc.cores = 1, mc.silent = TRUE)
    }
    
    # Stack Overflow fix
    # http://stackoverflow.com/questions/18504559/twitter-data-analysis-error-in-term-document-matrix
    #corpus.dat <- tm_map(corpus.dat, wordStem, language = "english", mc.cores = 1, mc.silent = TRUE)
    
    # task 2-1
    # Adapted from "Word, Pair and Triplet Frequencies" by Jadyen MacRae
    # Bug fix -- doesn't really work
    # http://jaydenmacrae.blogspot.com/2013/12/word-pair-and-triplet-frequencies.html
    # http://stackoverflow.com/questions/20577040/error-in-simple-triplet-matrix-unable-to-use-rweka-to-count-phrases
    ngt <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 3))}
    corpus.tdm <- TermDocumentMatrix(corpus.dat, control = list(tokenize = ngt))
    
    if (use.cache & !file.exists(cache.corpus.tdm)) {
      save(corpus.tdm, file = cache.corpus.tdm)
    }
  }
  return(corpus.tdm)
}

#' Read in a text file
#' 
#' @param full.path fully qualified path of file to read
#' @param line.count number of lines to read
#' @param sampled Boolean to indicate if lines are read randomly
#' @return text contents of \code{path}
#' @examples
#' get_file("myfile.txt")
#' get_file("myfile.txt", line.count = 1000, sampled = TRUE)
get_file <- function(full.path, line.count = -1, sampled = TRUE, use.cache = TRUE) {
  if (use.cache & file.exists(cache.text.dat)) {
    load(cache.text.dat, envir = environment())
  } else {
    con <- file(full.path, open = "rt")
    if (line.count == -1) {
      text.dat <- readLines(con)
    } else {
      # task 1-3
      if (sampled) {
        close(con)
        return(sample_lines(full.path, line.count))
      } else {
        text.dat <- readLines(con, n = line.count)
      }
    }
    close(con)
    
    # Stack Overflow fix
    # http://stackoverflow.com/questions/25699340/termdocumentmatrix-sometimes-throwing-error
    # this doesn't seem to really fix anything
    text.dat <- iconv(text.dat, to = "utf-8-mac", sub = "")
    text.dat <- (text.dat[!is.na(text.dat)])
    
    if (use.cache & !file.exists(cache.text.dat)) {
      save(text.dat, file = cache.text.dat)
    }
  }
  return(text.dat)
}

#' Sample text from multiple files
#' 
#' @param path path to files
#' @param pattern match files
#' @param line.count number of lines to read
#' @param sampled Boolean to indicate if lines are read randomly
#' @return text contents of files in \code{path}
#' @examples
#' get_files(".", "\\.+.txt")
#' get_files("my/folder", "myfile.txt", line.count = 1000, sampled = TRUE)
get_files <- function(path, pattern, line.count = 100, sampled = TRUE, use.cache = FALSE) {
  text.dat <- NULL
  for(f in dir(path, pattern)) {
    this.file <- get_file(paste0(path, "/", f), line.count, sampled, use.cache)
    
    if (is.null(text.dat)) {
      text.dat <- this.file
    } else {
      text.dat <- c(text.dat, this.file)
    }
  }
  return(text.dat)
}

#' Test if an input is an empty character vector
#' 
#' @param t a variable
#' @return boolean
#' @examples
#' is_empty_string("")
#' is_empty_string("not empty")
#' is_empty_string(c("not", "empty"))
is_empty_string <- function(t) {
  if (class("t") == "character") {
    if (length(t) == 1) {
      if (t[1] == "") {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

#' Delete cached files
#' 
#' @return return true if any files were removed
#' @examples
#' flush_cache()
flush_cache <- function() {
  did.flush <- FALSE
  if (file.exists(cache.corpus.dat)) {
    file.remove(cache.corpus.dat)
    did.flush <- TRUE
  }
  if (file.exists(cache.corpus.tdm)) {
    file.remove(cache.corpus.tdm)
    did.flush <- TRUE
  }
  if (file.exists(cache.text.dat)) {
    file.remove(cache.text.dat)
    did.flush <- TRUE
  }
  
  return(did.flush)
}

#' Do the work
#' @param text.in data to process if not from file list
#' @param line.count read only first \code{line.count} lines of each file
#' @param sampled BOOLEAN indicates whether to randomize lines read in corpus
#' @param rdata.out file to save memory environment
#' @param global.out base name to push data out into the Global environment
#' @param flush.cache delete previously cached term document matrices
#' @param use.cache use previously cached term document matrices
#' @param detect.lang classify language if true
#' @param use.stemming use English stemmer if true
#' @return Nothing returned
#' @examples
#' readCorpus_main(debug = TRUE)
readCorpus_main <- function(text.in = "",
                            line.count = 1000, 
                            sampled = TRUE, 
                            rdata.out = "output.RData",
                            global.out = "corpus",
                            flush.cache = FALSE,
                            use.cache = TRUE,
                            detect.lang = FALSE,
                            use.stemming = TRUE) {
  if (flush.cache) {
    flush_cache()
  }
  
  if (is_empty_string(text.in)) {
    path.en <- "final/en_US"
    files.en <- dir(path = path.en, pattern = "en_.+\\.txt")
  } else {
    files.en <- c("dummy.list")
    #sampled <- FALSE
    #flush.cache <- FALSE
    #use.cache <- FALSE
  }
  
  # this elaborate process is needed because there
  # seems to be no obvious way to avoid the error sometimes
  # caused by creating the Term Document Matrix
  # http://stackoverflow.com/questions/18504559/twitter-data-analysis-error-in-term-document-matrix
  # re-processing files can be really slow!
  corpus <- NULL
  if (sampled) {
    TRIES <- 3
  } else {
    TRIES <- 1
  }
  
  for(f in files.en) {
    
    corpus.tdm <- NULL
    corpus.tdm.use.cache <- TRUE
    while (is.null(corpus.tdm) & TRIES > 0) {
      # adapted from Machine Learning for Hackers, Drew Conway & John Myles White
      # Chapter 3, p.80 
      
      if (line.count == -1) {
        this.line.count <- -1
      } else {
        this.line.count <- floor(line.count / length(files.en))
      }
      
      if (is_empty_string(text.in)) {
        text.dat <- get_file(paste0(path.en, "/", f), 
                             this.line.count, 
                             sampled,
                             use.cache & corpus.tdm.use.cache)
      } else {
        text.dat <- text.in
      }
      
      corpus.dat <- get_corpus_dat(text.dat, 
                                   use.cache & corpus.tdm.use.cache)
      
      result = tryCatch({
        corpus.tdm <- get_corpus_tdm(corpus.dat, 
                                     use.cache & corpus.tdm.use.cache, 
                                     use.stemming)
      }, warning = function(w) {
        #warning-handler-code
      }, error = function(e) {
        TRIES <- TRIES - 1
        corpus.tdm.use.cache <- FALSE
        write(paste0("Error processing ", f, " TRIES = ", TRIES), stderr())
      }, finally = {
        #cleanup-code
      })
    }
    
    if (is.null(corpus.tdm)) {
      if (TRIES == 0) {
        stop(paste0("Unable to process corpus, failing on file ", f))
      } else {
        write(paste0("Failed to process ", f, " after max tries"), stderr())
        next
      }
    }
    
    # save some memory by filtering out really sparse terms
    # https://www.kaggle.com/c/job-salary-prediction/forums/t/4076/large-scale-text-mining-in-r/80253
    #corpus.tdm <- removeSparseTerms(corpus.tdm, sparse= 0.80)
    corpus.matrix <- as.matrix(corpus.tdm)
    corpus.wc <- rowSums(corpus.matrix)
    corpus.df <- data.frame(cbind(names(corpus.wc),
                                  as.numeric(corpus.wc)),
                            stringsAsFactors = FALSE)
    names(corpus.df) <- c("word", "freq")
    corpus.df$freq <- as.numeric(corpus.df$freq)
    corpus.df$ngram <- sapply(gregexpr("\\S+", corpus.df[, c("word")]), length)
    
    # task 2-3
    if (detect.lang) {
      corpus.df$lang <- textcat(corpus.df$word)
    }
    
    if (is_empty_string(text.in)) {
      corpus.df$fn <- f
    }
    
    if(is.null(corpus)) {
      corpus <- corpus.df
    } else {
      corpus <- rbind(corpus, corpus.df)
    }
  }
  #corpus <- corpus[complete.cases(corpus), ]
  
  if(!is.null(rdata.out)) {
    assign(paste0(global.out, ".", "df"), corpus, envir = .GlobalEnv)
    save.image(rdata.out)
  }
}

if (exists("readCorpus.debug", envir = globalenv())) {
  readCorpus_main(text.in = get_files("final/en_US/", ".*\\.txt", line.count = 1500))
}
