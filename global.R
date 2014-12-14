library("NLP")
library("openNLP")
library("tm")
library("RWeka")
library("qdap")
library("ggplot2")
library("stringdist")

c1 <- file("en_US.twitter.txt","r")
f1 <- readLines(c1, 3000)
c2 <- file("en_US.blogs.txt","r")
f2 <- readLines(c2, 1000)
c3 <- file("en_US.news.txt","r")
f3 <- readLines(c3, 1000)
close(c1)
close(c2)
close(c3)
f <- paste(f1,f2,f3, collapse= " ")
f <- iconv(f, "latin1", "ASCII", sub="")
corpus <- VCorpus(VectorSource(f )) 
#clean the data
corpus <- tm_map(corpus, removeNumbers) 
corpus <- tm_map(corpus, stripWhitespace) 
corpus <- tm_map(corpus, tolower) 
corpus <- tm_map(corpus, removePunctuation) 
corpus <- tm_map(corpus, removeWords, stopwords("english")) #remove stop words

#free memory
remove(f)
remove(f1)
remove(f2)
remove(f3)



d1 <- DocumentTermMatrix(corpus)
freq <- sort(colSums(as.matrix(d1)), decreasing=TRUE)
s1 <- data.frame(word=names(freq), freq=freq) # single word table
s1$row.names <- NULL
s1$freq <- NULL
#free memory
remove(d1)
remove(freq)


BigramTokenizer <- function(x) NGramTokenizer(x, 
                                              Weka_control(min = 2, max = 2))
d2 <- DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer))
freq <- sort(colSums(as.matrix(d2)), decreasing=TRUE)
s2 <- data.frame(word=names(freq), freq=freq)

s2$row.names <- NULL
s2$freq <- NULL
remove(d2)
remove(freq)

print("one gram finished")

TrigramTokenizer <- function(x) NGramTokenizer(x, 
                                               Weka_control(min = 3, max = 3))


d3 <- DocumentTermMatrix(corpus, control = list(tokenize = TrigramTokenizer))
freq <- sort(colSums(as.matrix(d3)), decreasing=TRUE)
s3 <- data.frame(word=names(freq), freq=freq)

s3$row.names <- NULL
s3$freq <- NULL
remove(d3)
remove(freq)
print("tri gram finished")
print("four gram building")
FourgramTokenizer <- function(x) NGramTokenizer(x, 
                                               Weka_control(min = 4, max = 4))



d4 <- DocumentTermMatrix(corpus, control = list(tokenize = FourgramTokenizer ))
freq <- sort(colSums(as.matrix(d4)), decreasing=TRUE)
s4 <- data.frame(word=names(freq), freq=freq)
s4$row.names <- NULL
s4$freq <- NULL
print("four gram building finished")
remove(d4)
remove(freq)
remove(corpus)
print("four gram finished")

predictcurr <- function(data=s1, sent)
{
   
    sent <- preprc(sent)
    words <- populate(sent)
    prediction <- predict0(,words[length(words)])
    prediction
   
    
}

predictwords <- function(data=s4, sent) 
{
   
    
    sent <- preprc(sent)
    words <- populate(sent)
    sentlen <- length(words)
    
    if(sentlen >= 3) 
    {
        ln <- length(words)
        prediction <- predict3(, words[ln-2], words[ln-1], words[ln])
        if(length(prediction) == 0) {
            sentlen <- sentlen -1
            words[1] <- words[2]
            words[2] <- words[3]
        }
        
    }
    if(sentlen == 2) 
    {
    
        prediction <- predict2(, words[1], words[2])
    
        if(length(prediction) == 0) {
            sentlen <- sentlen -1
            words[1] <- words[2]
        }
    }
    if (sentlen== 1)
    {
        prediction <- predict1(,words[1])
        if(length(prediction) == 0)
            prediction <- predict0(,words[1])
        
    }
    if(length(prediction) > 3)
        prediction <- prediction[seq(1:4)]
    prediction
    
}


predict3 <- function(data=s4, x,y,z)
{
   
    sent <- paste(x,y,z, sep=" ")
    sent <- trim(sent)
    patt <- paste("^",sent, sep= "")
    found <- grep(patt, data$word)
  
    extractwords(data$word[found], sent)
    
}

trim <- function( x ) {
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}
predict2 <- function(data=s3, x,y)
{
    
    sent <- paste(x,y, sep=" ")
    sent <- trim(sent)
    patt <- paste("^",sent, sep= "")
    found <- grep(patt, data$word)
  
    extractwords(data$word[found], sent)
    
}
predict1 <- function(data=s2, x)
{
    
    sent <- x
    sent <- trim(sent)
    patt <- paste("^",sent, sep= "")
    found <- grep(patt, data$word)
    extractwords(data$word[found], sent)
    
}


predict0 <- function(data=s1, x)
{
    
    sent <- x
    sent <- trim(sent)
    found <- amatch(x,s1$word,maxDist=2, nomatch=0)
    if(found != 0)
        extractwords(data$word[found], sent)
    else
        as.character(data$word[1], sent)
    
}


extractwords <- function(words, sent)
{
    words <- as.character(words)
    w <- character()
    output <- w
    if(length(words) != 0)
    { 
  
        l <- length(words)
        w <- character()
        s <- strsplit(sent, " ")[[1]]
        slen <- length(s)
        for(i in 1:l)
        {
            lastw <- strsplit(words[[i]], " ")[[1]]
            slen <- length(s)
            #while (slen != 0){
                #slen = slen -1
                w <- append(w, lastw[length(lastw)])
            #}
    
        }
        output <- w
    }
    output
}

populate <- function(sent)
{
    words <- strsplit(sent, " ")[[1]]
    
    words
        
}

preprc <- function(x=sent)
{
    c <- VCorpus(VectorSource(x )) 
    #clean the data
    c <- tm_map(c, removeNumbers) 
    c <- tm_map(c, stripWhitespace) 
    c <- tm_map(c, tolower) 
    c <- tm_map(c, removePunctuation) 
    c <- tm_map(c, removeWords, stopwords("english")) #remove stop words
    Content(c[[1]])
}
