# I'm going to try to self-compile topics using unique words

library(XLConnect)
library(stringdist)
library(stringr)
library(tm)
library(RTextTools)
library(topicmodels)
raw_WHO <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/META_ALL_v4.2.xlsx"),sheet=4)
raw_WHO <- raw_WHO[!duplicated(raw_WHO[,2]),]
raw_WHO <- data.frame(code=raw_WHO$code,label=tolower(raw_WHO$label),org=raw_WHO$set)
raw_WHO$label <- str_replace_all(raw_WHO$label, "[[:digit:]]", " ")
raw_WHO$label <- str_replace_all(raw_WHO$label, "[^[:alnum:]]", " ")
zombie <- VCorpus(VectorSource(raw_WHO$label))
zombie2 <- tm_map(zombie, removeWords, stopwords("english"))
zombie2 <- tm_map(zombie, stemDocument, language="english")
tdm <- TermDocumentMatrix(zombie2, control = list(weighting = weightTfIdf))
tdm_deux <- as.matrix(tdm)
tdm_deux2 <- sort(rowSums(tdm_deux),decreasing=TRUE)
word_count <- data.frame(word=names(tdm_deux2),freq=tdm_deux2)
top_fifty2 <- word_count[1:50,]

tdm2 <- TermDocumentMatrix(zombie2)
tdm_deux5 <- as.matrix(tdm2)
tdm_deux52 <- sort(rowSums(tdm_deux5),decreasing=TRUE)
word_count2 <- data.frame(word=names(tdm_deux52),freq=tdm_deux52)
top_fifty <- word_count2[1:50,]

stem_WHO <- raw_WHO
stem_WHO$label <- data.frame(text=unlist(sapply(zombie2, `[`, "content")), stringsAsFactors=FALSE)
small_WHO <- stem_WHO[1:200,]

small_talk_jw <- function(x)
{
  candice <- data.frame()
  x_men <- 1:nrow(x)
  for(i in x_men)
  {
    y_men <- x_men[x_men!=i]
    henry <- data.frame()
    for(j in y_men)
    {
      dave <- data.frame(orig=x[i,2],org1=x[i,3],match=x[j,2],org2=x[j,3],dist=NA)
      dave[1,5] <- stringdist(dave[1,1],dave[1,3],method="jw",p=0)
      henry <- rbind(henry, dave)
    }
    elsa <- henry[order(henry$dist),]
    elsa <- elsa[1,]
    child1 <- data.frame(child1=strsplit(paste(elsa[,1])," "),match=NA)
    child2 <- data.frame(child2=strsplit(paste(elsa[,3])," "))
    for(m in seq(nrow(child1)))
    {
      for(w in seq(nrow(child2)))
      {
        if(paste(child1[m,1])==paste(child2[w,1]))
        {
          child1[m,2] <- 1
          break
        }
        else(child1[m,2] <- 0)
      }
    }
    child1_recomp <- paste(child1[child1[,2]==0,1],collapse=" ")
    if(nchar(paste(child1_recomp))<=1){child1_recomp <- paste(elsa[,1])}
    julie <- data.frame(orig=x[i,2],topic=child1_recomp,org=x[i,3])
    candice <- rbind(candice,julie)
  }
  return(candice)
}

small_talk_dl <- function(x)
{
  candice <- data.frame()
  x_men <- 1:nrow(x)
  for(i in x_men)
  {
    y_men <- x_men[x_men!=i]
    henry <- data.frame()
    for(j in y_men)
    {
      dave <- data.frame(orig=x[i,2],org1=x[i,3],match=x[j,2],org2=x[j,3],dist=NA)
      dave[1,5] <- stringdist(dave[1,1],dave[1,3],method="dl")
      henry <- rbind(henry, dave)
    }
    elsa <- henry[order(henry$dist),]
    elsa <- elsa[1,]
    child1 <- data.frame(child1=strsplit(paste(elsa[,1])," "),match=NA)
    child2 <- data.frame(child2=strsplit(paste(elsa[,3])," "))
    for(m in seq(nrow(child1)))
    {
      for(w in seq(nrow(child2)))
      {
        if(paste(child1[m,1])==paste(child2[w,1]))
        {
          child1[m,2] <- 1
          break
        }
        else(child1[m,2] <- 0)
      }
    }
    child1_recomp <- paste(child1[child1[,2]==0,1],collapse=" ")
    if(nchar(paste(child1_recomp))<=1){child1_recomp <- paste(elsa[,1])}
    julie <- data.frame(orig=x[i,2],topic=child1_recomp,org=x[i,3])
    candice <- rbind(candice,julie)
  }
  return(candice)
}

system.time(whatever_test_jw <- small_talk_jw(small_WHO))
system.time(whatever_test_dl2 <- small_talk_dl(whatever_test_jw))

whatever_final <- data.frame(org=small_WHO[,3],orig=raw_WHO[1:200,2],tags_jw=whatever_test_jw[,2],tags_lv=whatever_test_dl2[,2])

matrix_WHO <- create_matrix(cbind(as.vector(whatever_final[,2]),as.vector(whatever_final[,3]),as.vector(whatever_final[,4])), language="english", stemWords=TRUE,weighting=tm::weightTfIdf)

raw_WHO[263,2] <- "I want cheerios like right now"
raw_WHO[1307,2] <- "Also popcorn actually I hate popcorn"
matrix_WHO <- create_matrix(na.omit(raw_WHO$label), language="english", stemWords=TRUE,weighting=weightTfIdf)

row_totes_WHO <- apply(matrix_WHO, 1, sum)
matrix_WHO <- matrix_WHO[row_totes_WHO > 0,]
lda_WHO <- LDA(matrix_WHO,15)
terms(lda_WHO)
topics(lda_WHO)

whatev <- VCorpus(VectorSource(whatever_final[,2]))
whatev2 <- tm_map(whatev, content_transformer(tolower))
whatev2 <- tm_map(whatev2, removeWords, stopwords("english"))
whatev2 <- tm_map(whatev2, removePunctuation)
whatev2 <- tm_map(whatev2, removeNumbers)
whatev2 <- tm_map(whatev2, stemDocument, language="english")
dtm <- DocumentTermMatrix(whatev2, control = list(weighting = weightTfIdf))
dtm_deux <- as.matrix(dtm)
dtm_deux2 <- sort(rowSums(dtm_deux),decreasing=TRUE)
word_count <- data.frame(word=names(dtm_deux2),freq=dtm_deux2)
top_fifty2 <- word_count[1:50,]
