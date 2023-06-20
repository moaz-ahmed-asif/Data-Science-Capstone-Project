options(timeout = 6000, mc.cores = 1)

library(downloader)
library(plyr)
library(dplyr)
library(stringi)
library(tm)
library(RWeka)

if(!file.exists("./Coursera-SwiftKey.zip")){
        download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                      destfile="./Coursera-SwiftKey.zip")
}

if(!file.exists("./final")) { unzip("./Coursera-SwiftKey.zip") }
list.files("./final")

en_tw <- file("./final/en_US/en_US.twitter.txt")
length(en_tw)
lineTw <- readLines(en_tw)

file.size("./final/en_US/en_US.blogs.txt") / 1024 / 1024
length(lineTw)

max(stri_length(lineTw))

en_bl <- file("./final/en_US/en_US.blogs.txt")
lineBl <- readLines(en_bl)
max(stri_length(lineBl))

en_ne <- file("./final/en_US/en_US.news.txt")
lineNe <- readLines(en_ne)
max(stri_length(lineNe))

length(grep("love", lineTw)) / length(grep("hate", lineTw))

lineTw[grep("biostats", lineTw)]
length(grep("A computer once beat me at chess, but it was no match for me at kickboxing", lineTw))

library(tm)
set.seed(2023-06-11)
data.sample <- c(sample(lineTw, length(lineTw) * 0.02),
                 sample(lineNe, length(lineNe) * 0.02),
                 sample(lineBl, length(lineBl) * 0.02))

corpus <- VCorpus(VectorSource(data.sample))
toSpace1 <- function(x) gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", " ", x)
corpus <- tm_map(corpus, content_transformer(toSpace1) )
toSpace2 <- function(x) gsub("@[^\\s]+", " ", x)
corpus <- tm_map(corpus, content_transformer(toSpace2))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)

# saveRDS(corpus, file = "final_data_corpus.RData")
final_data_corpus <- data.frame(text = unlist(sapply(corpus,`[`, "content")), stringsAsFactors = F)

gc()

getGramDf <- function(df, n) {
        gram <- NGramTokenizer(df, Weka_control(min = n, max = n))
        gram_df <- data.frame(table(gram))
        names(gram_df) <- c("words","freq")
        gram_df <- gram_df[order(gram_df$freq, decreasing = T),]
        return(gram_df)
}

getUnigram <- function(df) {
        gram_df <- getGramDf(df, 1)  
        gram_df$words <- as.character(gram_df$words)
        word_split <- strsplit(gram_df$words, split= " ")
        unigram <- transform(gram_df, word1 = sapply(word_split, "[[", 1))
        # saveRDS(unigram, "unigram.RData")
        return(unigram)
}

getBigram <- function(df) {
        gram_df <- getGramDf(df, 2)  
        gram_df$words <- as.character(gram_df$words)
        word_split <- strsplit(gram_df$words, split= " ")
        bigram <- transform(gram_df, word1 = sapply(word_split, "[[", 1), word2 = sapply(word_split, "[[", 2))
        saveRDS(bigram, "bigram.RData")
        return(bigram)
}

getTrigram <- function(df) {
        gram_df <- getGramDf(df, 3)  
        gram_df$words <- as.character(gram_df$words)
        word_split <- strsplit(gram_df$words, split= " ")
        trigram <- transform(gram_df, word1 = sapply(word_split, "[[", 1), word2 = sapply(word_split, "[[", 2), word3 = sapply(word_split, "[[", 3))
        saveRDS(trigram, "trigram.RData")
        return(trigram)
}

getQuadgram <- function(df) {
        gram_df <- getGramDf(df, 4)  
        gram_df$words <- as.character(gram_df$words)
        word_split <- strsplit(gram_df$words, split= " ")
        quadgram <- transform(gram_df, word1 = sapply(word_split, "[[", 1), word2 = sapply(word_split, "[[", 2), word3 = sapply(word_split, "[[", 3), word4 = sapply(word_split, "[[", 4))
        saveRDS(quadgram, "quadgram.RData")
        return(quadgram)
}

gc()
startingTime <- proc.time()
unigram <- getUnigram(final_data_corpus)
bigram <- getBigram(final_data_corpus)
trigram <- getTrigram(final_data_corpus)
quadgram <- getQuadgram(final_data_corpus)
proc.time() - startingTime
gc()

average_frequency<-function(sentence)  {
        sentence_c<-removeWords(sentence,stopwords('en'))
        sentence_c<-stripWhitespace(removeNumbers(removePunctuation(tolower(sentence_c),preserve_intra_word_dashes = TRUE)))
        sentence_split<- strsplit(sentence_c," ")[[1]]
        qwords<-length(sentence_split)
        tot_frequency<-0
        for (i in c(1,2,3,4)) {
                weight_i<-i/10
                last_words<-tail(sentence_split,i)
                
                if(i==1) {
                        freq<-as.integer(head((unigram[unigram$word1==last_words[1],])$freq,1))
                }
                else if(i==2) {
                        freq<-as.integer(head((bigram[bigram$word1==last_words[1] 
                                                      & bigram$word2 == last_words[2]                               
                                                      ,])$freq,1))
                        
                }
                else if(i==3) {
                        freq<-as.integer(head((trigram[trigram$word1==last_words[1] 
                                                       & trigram$word2 == last_words[2]
                                                       & trigram$word3 == last_words[3]
                                                       ,])$freq,1))
                }
                else if(i==4) {
                        freq<-as.integer(head((quadgram[quadgram$word1==last_words[1] 
                                                        & quadgram$word2 == last_words[2]
                                                        & quadgram$word3 == last_words[3]
                                                        & quadgram$word4 == last_words[4]
                                                        ,])$freq,1))
                        
                }
                
                if(length(freq)==0) freq<-0
                tot_frequency<-tot_frequency+(weight_i*freq)
        }
        return(tot_frequency)
}

find_frequency <- function(sentence, options) {
        for(i in 1:length(options)) {
                sentence_n<-paste(sentence, options[i])
                print(sentence_n)
                a<-average_frequency(sentence_n)
                print(a)
        }
}

find_frequency("The guy in front of me just bought a pound of bacon, a bouquet, and a case of", c('pretzels', 'cheese', 'beer', 'soda'))

find_frequency("You're the reason why I smile everyday. Can you follow me please? It would mean the", c('world', 'best', 'most', 'universe'))

find_frequency("Hey sunshine, can you follow me and make me the", c('bluest','happiest', 'smelliest', 'saddest'))

find_frequency("Very early observations on the Bills game: Offense still struggling but the", c('players','defense', 'referees', 'crowd'))

find_frequency("Go on a romantic date at the", c('movies','mall', 'grocery', 'beach'))

find_frequency("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", c('motorcycle','way', 'horse', 'phone'))

find_frequency("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", c('time','thing', 'years', 'weeks'))

find_frequency("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", c('ears','eyes', 'toes', 'fingers'))

find_frequency("Be grateful for the good times and keep the faith during the", c('worse','bad', 'hard', 'sad'))

find_frequency("If this isn't the cutest thing you've ever seen, then you must be", c('asleep','callous', 'insane', 'insensitive'))



find_frequency("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd", c('give','eat','die','sleep'))

find_frequency("Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his", c('marital','spiritual','horticultural','financial'))

find_frequency("I'd give anything to see arctic monkeys this", c('month','decade','weekend','morning'))

find_frequency("Talking to your mom has the same effect as a hug and helps reduce your", c('stress','sleepiness','hunger','happiness'))

find_frequency("When you were in Holland you were like 1 inch away from me but you hadn't time to take a", c('picture', 'minute', 'look', 'walk'))

find_frequency("I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the", c('incident','case','matter','account'))

find_frequency("I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each", c('finger','arm','toe','hand'))

find_frequency("Every inch of you is perfect from the bottom to the", c('middle','side','center','top'))

find_frequency("I’m thankful my childhood was filled with imagination and bruises from playing", c('outside','daily','inside','weekly'))

find_frequency("I like how the same people are in almost all of Adam Sandler's", c('stories','pictures','novels','movies'))

