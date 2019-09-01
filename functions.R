
library(tm)
library(NLP)
library(stringr)
library(dplyr)
library(data.table)



# read training data files to configure the app
df1 <- readRDS("word_1.rds") 
df2 <- readRDS("word_2.rds") 
df3 <- readRDS("word_3.rds") 
df4 <- readRDS("word_4.rds") 
      
msg <-" "      
# Romove 'profanity' which was previosly saved
removeProfanity <- function(str) {
        dfProf <- readRDS("profanity.rds")
        nWords <- nrow(dfProf)
        for(i in 1:nWords) 
                str <- gsub(dfProf[i,1]," ",str, perl=TRUE) 
        return(str)
}


Clean<- function(inputString)
{
        # cleaning up data
        inputString<- iconv(inputString, "latin1", "ASCII", sub=" ");
        inputString<- gsub("[^[:alpha:][:space:][:punct:]]", "", inputString);
        # corpus
        input_corpus<- VCorpus(VectorSource(inputString))
        input_corpus<- tm_map(input_corpus, content_transformer(tolower))
        input_corpus<- tm_map(input_corpus, removePunctuation)
        input_corpus<- tm_map(input_corpus, removeNumbers)
        input_corpus<- tm_map(input_corpus, stripWhitespace)
        inputString<- as.character(input_corpus[[1]])
        inputString<- gsub("(^[[:space:]]+|[[:space:]]+$)", "", inputString) 
        inputString <- removeProfanity(inputString)
        if (nchar(inputString) > 0) {
                return(inputString); 
        } else {
                return("");
        }
}


# The objective of this function is find the most probable word in a n-gram dataframe , given a group of n last words
##Prediction with Katz Back-off model

predict_next_word<- function(inputString)
{
        # Data cleansing using the function written earlier
        inputString <- Clean(inputString);
        # extract the string length
        inputString <- unlist(strsplit(inputString, split=" "));
        inputStringLen <- length(inputString);
        nextWordPresent <- FALSE;
        termNext <- as.character(NULL);
        # backoff N-gram model
        if (inputStringLen >= 3 & !nextWordPresent)
        {
                # collate the terms
                inputString1 <- paste(inputString[(inputStringLen-2):inputStringLen], collapse=" ");
                # take the subset of 4-gram data
                searchStr <- paste("^",inputString1, sep = "");
                df4Temp <- df4[terms %like% searchStr][order(-frequency)];
                if (df4Temp[, .N] > 1 )  # check if there are more than 1 match
                {
                        termNext <- df4Temp[1:5,1];# select top 5 matching terms
                        nextWordPresent <- TRUE;
                }
                df4Temp <- NULL;
        }
        # n-1 gram -- checking 3-gram model if there were not more than 1 match with the 4-gram
        if (inputStringLen>= 2 & !nextWordPresent)
        {
                # collate input terms
                inputString1 <- paste(inputString[(inputStringLen-1):inputStringLen], collapse=" ");
                searchStr <- paste("^",inputString1, sep = "");
                df3Temp <- df3[terms %like% searchStr][order(-frequency)];
                if ( df3Temp[, .N] > 1 )
                {
                        termNext <- df3Temp[1:5,1];  # select top 5 matching terms
                        nextWordPresent <- TRUE;
                }
                df3Temp <- NULL;
        }
        # ngram -- checking the 2 gram model for second to last word
        if (inputStringLen >= 1 & !nextWordPresent)
        {
                inputString1 <- inputString[inputStringLen];
                searchStr <- paste("^",inputString1, sep = "");
                df2Temp <- df2[terms %like% searchStr][order(-frequency)];
                if ( df2Temp[, .N] > 1 )
                {
                        termNext<- df2Temp[1:5,1];  # select top 5 matching terms
                        nextWordPresent<- TRUE;
                }
                df2Temp <- NULL;
        }  
        
        if (nextWordPresent) 
        { 
                wordNxt <- termNext[,word(terms, -1)];
        }
        else wordNxt <- df1[order(-frequency)][1:5, 1];
        # final if statement to return the df of predicted words    
        if (inputStringLen > 0)
        {
                df <- data.frame(wordNxt);
                return(df);
        } else 
        {
                wordNxt <- df1[order(-frequency)][1:5, 1];
                df <- data.frame(wordNxt);
                return(df);
        }
}

