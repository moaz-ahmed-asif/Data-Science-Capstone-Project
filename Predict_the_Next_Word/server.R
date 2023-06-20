library(shiny)
library(stringr)
library(tm)
library(NLP)

# Loading bigram, trigram and quadgram frequencies words matrix frequencies
bigram <- readRDS("bigram.RData")
trigram <- readRDS("trigram.RData")
quadgram <- readRDS("quadgram.RData")

# function for searching in bigram frequency table.
next_word_bigram <- function(sentence_split) {
    next_word <- as.character(head((bigram[bigram$word1 == sentence_split[1], ])$word2, 1))
    freq <- as.character(head((bigram[bigram$word1 == sentence_split[1], ])$freq, 1))
    if(identical(next_word, character(0))) {
        next_word <- NA
        freq <- 0
    }
    next_word_list <- list(next_word, freq)
    return(next_word_list)
}

#function for searching in trigram frequency table.
#if it not found, it use the function for look in bigram
next_word_trigram <- function(sentence_split) {
    next_word <- as.character(head((trigram[trigram$word1 == sentence_split[1]
                                            & trigram$word2 == sentence_split[2], ])$word3, 1))
    freq <- as.character(head((trigram[trigram$word1 == sentence_split[1] 
                                       & trigram$word2 == sentence_split[2], ])$freq, 1))
    next_word_list <- list(next_word, freq)
    if(identical(next_word, character(0))) {
        next_word_list = next_word_pred(sentence_split[2])
    }
    return(next_word_list)
}

#function for searching in quadgram frequency table.
#if it not found, it use the function for look in trigram
next_word_quadgram <- function(sentence_split) {
    next_word <- as.character(head((quadgram[quadgram$word1 == sentence_split[1] 
                                             & quadgram$word2 == sentence_split[2]
                                             & quadgram$word3 == sentence_split[3], ])$word4, 1))
    freq<-as.integer(head((quadgram[quadgram$word1 == sentence_split[1] 
                                    & quadgram$word2 == sentence_split[2]
                                    & quadgram$word3 == sentence_split[3], ])$freq, 1))
    next_word_list<-list(next_word, freq)
    if(identical(next_word, character(0))) {
        next_word_list = next_word_pred(paste(sentence_split[2], sentence_split[3], sep = " "))
    }
    return(next_word_list)
}

#function for searching the sentence.
next_word_pred <- function(sentence, ngrams_words = 0)  {
    sentence_c <- stripWhitespace(removeNumbers(tolower(sentence), preserve_intra_word_dashes = T))
    sentence_split <- strsplit(sentence_c, " ")[[1]]
    qwords <- length(sentence_split)
    if(qwords == 1 || ngrams_words == 2) { 
        next_word_list <- next_word_bigram(tail(sentence_split, 1)) #use bigram to find out next word
    }  
    else if(qwords == 2 || ngrams_words == 3) { 
        next_word_list <- next_word_trigram(tail(sentence_split, 2)) #use trigram to find out next word
    }
    else if(qwords > 2 || ngrams_words == 3) {
        next_word_list <- next_word_quadgram(tail(sentence_split, 3)) #use quadgram to find out next word
    }
    else {
        next_word_list <- list(NA, 0)
    }
    return(next_word_list)
}

testtime <- function(sentence_split, ngrams_words = 0) {
    ptm <- proc.time()
    next_word_pred(sentence_split, 0)
    t <- proc.time() - ptm
    return(t['elapsed'])
}

# Define server logic
shinyServer(function(input, output) {
    output$next_word <- renderText({
        result <- next_word_pred(input$inputText, 0)
        result[[1]]});
    
    output$time <- renderText(testtime(input$inputText));
    
    output$bigram <- renderText({
        result <- next_word_pred(input$inputText, 2)
        result[[1]]});
    
    output$trigram <- renderText({
        result <- next_word_pred(input$inputText, 3)
        result[[1]]});
    
    output$quadgram <- renderText({
        result <- next_word_pred(input$inputText, 4)
        result[[1]]});
})