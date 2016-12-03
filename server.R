require("RcppRoll")
require("httpuv")
require("jsonlite")
require("shiny")
require("xtable")

if("RcppRoll" %in% rownames(installed.packages()) == FALSE) {install.packages("RcppRoll")}
library(RcppRoll)
if("httpuv" %in% rownames(installed.packages()) == FALSE) {install.packages("RcppRoll")}
library(httpuv)
if("jsonlite" %in% rownames(installed.packages()) == FALSE) {install.packages("RcppRoll")}
library(jsonlite)
if("shiny" %in% rownames(installed.packages()) == FALSE) {install.packages("RcppRoll")}
library(shiny)
if("xtable" %in% rownames(installed.packages()) == FALSE) {install.packages("RcppRoll")}
library(xtable)
library(stringi)
library(ngram)


shinyServer(  
  function(input, output) {    
    #############################
    # load text process functions
    # remove URLs
    removeURL <- function(x)  {
      x <- gsub("((f|ht)tp[^ ]*)", "", x) # start with either ftp or http
      x <- gsub("([^ ]*.com)", "", x) # end with .com
      return(x)
    }
    # remove non-ascii characters and punctuations
    removePunc <- function(x) stri_replace_all_regex(x, "\\p{P}", " ")
    preproc <- function(x) {
      x <- removeURL(x)
      x <- removePunc(x)
      x <- preprocess(x, case="lower", remove.punct = TRUE, remove.numbers = TRUE, fix.spacing = TRUE) #                package "ngram"
      return (x)
    }
    
    #################################
    # load ngram prediction functions
    predict.uni <- function() {
      uni.tbl="uni.tbl.csv"
      inputFile <- uni.tbl
      con <- file(inputFile, open='r')
      # skip header
      oneLine <- readLines(con, n=1)
      s <- ""
      for (i in 1:5) {
        oneLine <- readLines(con, n=1)
        myLine <- unlist(strsplit(oneLine,"[_,]"))
        if (s=="") {
          s <- myLine[1]
        }
        else {
          s <- paste(s, myLine[1], sep=", ")
        }
      }
      close(con)
      return(s)
    }
    
    predict.bi <- function(aWord) {
      # load dictionary index for quick lookup
      bi.idxLetter <- "bi.idxLetter.csv"
      
      inputFile <- bi.idxLetter
      con <- file(inputFile, open='r')
      lines <- readLines(con)
      # print(lines)
      close(con)
      idxLetter <- as.numeric(lines[2:length(lines)]) # skip header
      # print(idxLetter)
      
      # read bigram table
      bi.tbl <- "bi.tbl.csv"
      
      inputFile <- bi.tbl
      con <- file(inputFile, open='r')
      lines <- readLines(con) # including header
      close(con)
      
      # new scheme to speed up: get an alphatic index lookup table
      #  for first alphabat
      # aWord <- "of" # for dev
      firstLetter <- substr(aWord,1,1)
      idx <- which(letters==firstLetter)
      if (length(idx)>0) {
        startL <- idxLetter[idx]+1
        endL <- idxLetter[idx+1]
        # print(letters)
        # print(idxLetter)
        # print(startL)
        # print(endL)
        
        lines <- lines[startL:endL]
        # str(lines)
        
        ss <- strsplit(lines,"[_,]")
        # str(ss)
        # head(ss)
        word1 <- sapply(ss, "[", c(1))
        word2 <- sapply(ss, "[", c(2))
        ff <- sapply(ss, "[", c(5))
        
        idx2 <- which(word1==aWord)
        if (length(idx2)>0) {
          # if phrase found, return a maximum of five next word(s)
          #   #  sorted by descending order of freq
          s <- word2[idx2]
          f <- as.numeric(ff[idx2]) # freq
          #  sorted by descending order of freq
          s <- s[order(-f)]
          s <- s[1:min(length(s),5)]
          return(s)
        }
        else {
          # if phrase not found, return most frequent word(s)
          s <- predict.uni()
          return( s )
        }
      }
      else {
        # if phrase not found, return most frequent word(s)
        s <- predict.uni()
        return( s )
      }
      
    }
    
    predict.tri <- function(twoWords) {
      # load dictionary index for quick lookup
      tri.idxLetter <- "tri.idxLetter.csv"
      
      inputFile <- tri.idxLetter
      con <- file(inputFile, open='r')
      lines <- readLines(con)
      # print(lines)
      close(con)
      idxLetter <- as.numeric(lines[2:length(lines)]) # skip header
      # print(idxLetter)
      
      # read trigram table
      tri.tbl <- "tri.tbl.csv"
      
      inputFile <- tri.tbl
      con <- file(inputFile, open='r')
      lines <- readLines(con) # including header
      close(con)
      
      # new scheme to speed up: get an alphatic index lookup table
      #  for first alphabat
      # twoWords <- c("it", "really") # for dev
      firstLetter <- substr(twoWords[1],1,1)
      idx <- which(letters==firstLetter)
      if (length(idx)>0) {
        startL <- idxLetter[idx]+1
        endL <- idxLetter[idx+1]
        # print(letters)
        # print(idxLetter)
        # print(startL)
        # print(endL)
        
        lines <- lines[startL:endL]
        # str(lines)
        
        ss <- strsplit(lines,"[_,]")
        # str(ss)
        # head(ss)
        word1 <- sapply(ss, "[", c(1))
        word2 <- sapply(ss, "[", c(2))
        word3 <- sapply(ss, "[", c(3))
        ff <- sapply(ss, "[", c(6))
        
        idx2 <- which(word1==twoWords[1] & word2==twoWords[2])
        if (length(idx2) > 0 ) {
          # if phrase found, return a maximum of five next word(s)
          #   #  sorted by descending order of freq
          s <- word3[idx2]
          f <- as.numeric(ff[idx2]) # freq
          #  sorted by descending order of freq
          s <- s[order(-f)]
          s <- s[1:min(length(s),5)]
          return(s)
        }
        else {
          # if phrase not found, use bigram for prediction
          aWord <- twoWords[2]
          s <- predict.bi(aWord)
          return( s )
        }
      }
      else {
        # if phrase not found, use bigram for prediction
        aWord <- twoWords[2]
        s <- predict.bi(aWord)
        return( s )
      }
      
    }
    
    predict.quad <- function(threeWords) {
      # load dictionary index for quick lookup
      quad.idxLetter <- "quad.idxLetter.csv"
      
      inputFile <- quad.idxLetter
      con <- file(inputFile, open='r')
      lines <- readLines(con)
      # print(lines)
      close(con)
      idxLetter <- as.numeric(lines[2:length(lines)]) # skip header
      # print(idxLetter)
      
      # read quadgram table
      quad.tbl <- "quad.tbl.csv"
      
      inputFile <- quad.tbl
      con <- file(inputFile, open='r')
      lines <- readLines(con) # including header
      close(con)
      
      # new scheme to speed up: get an alphatic index lookup table
      #  for first alphabat
      # threeWords <- c("again", "it", "s") # for dev
      firstLetter <- substr(threeWords[1],1,1)
      idx <- which(letters==firstLetter)
      if (length(idx)>0) {
        # does not exist
        startL <- idxLetter[idx]+1
        endL <- idxLetter[idx+1]
        # print(letters)
        # print(idxLetter)
        # print(startL)
        # print(endL)
        
        lines <- lines[startL:endL]
        # str(lines)
        
        ss <- strsplit(lines,"[_,]")
        # str(ss)
        # head(ss)
        word1 <- sapply(ss, "[", c(1))
        word2 <- sapply(ss, "[", c(2))
        word3 <- sapply(ss, "[", c(3))
        word4 <- sapply(ss, "[", c(4))
        ff <- sapply(ss, "[", c(7))
        
        idx2 <- which(word1==threeWords[1] & word2==threeWords[2] & word3==threeWords[3])
        
        if (length(idx2) > 0 ) {
          # if phrase found, return a maximum of five next word(s)
          #   #  sorted by descending order of freq
          s <- word4[idx2]
          f <- as.numeric(ff[idx2]) # freq
          #  sorted by descending order of freq
          s <- s[order(-f)]
          s <- s[1:min(length(s),5)]
          return(s)
        }
        else {
          # if phrase not found, use bigram for prediction
          twoWords <- c(threeWords[2], threeWords[3])
          s <- predict.tri(twoWords)
          return( s )
        }
        
      }
      else {
        # if phrase not found, use bigram for prediction
        twoWords <- c(threeWords[2], threeWords[3])
        s <- predict.tri(twoWords)
        return( s )
      }
    }
    
    
    
    ntext <- eventReactive(input$goButton, {
      
      #################################
      # parse input text
      aString <- input$inputTxt
      
      # preprocess sentence
      s0 <- preproc(aString)
      # tokenize a string to feed to the predictors
      s0 <- unlist(strsplit(s0, "\\s+"))
      # obtain last three words for next word prediction
      if (length(s0)>=3) {
        threeWords <- tail(s0,n=3) # for predict.quad
        # predict next word
        s <- predict.quad(threeWords)
      }
      if (length(s0)==2) {
        twoWords <- s0 # for predict.tri
        # predict next word
        s <- predict.tri(twoWords)
      }
      if (length(s0)==1) {
        aWord <- s0 # for predict.bi
        # predict next word
        s <- predict.bi(aWord)
      }
      
      s<- noquote(paste(sprintf("%s", s), collapse=", "))
      
      # # tokenize input words and predict
      # input$inputTxt # this is a place holder
      
    })    
      
    output$nextWord <- renderText( {
        ntext()
    })
   
  }
)
    
