# New activity -- outcome scraping
# We'll be looking at the SDGs to do with health and agriculture

library(plyr)
library(tm)
library(stringr)
library(readstata13)
library(RWeka)

# narrow down by verb and then

targets <- read.csv("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Evaluation mining/Targets and goals1.2.csv")
dir <- "/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Evaluation mining/All the PDFs- Ghana/Projects/"
files <- data.frame(list.files("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Evaluation mining/All the PDFs- Ghana/Projects/",pattern=".pdf"))

PDF_pre <- readPDF(control=list(text="-raw"))(elem=list(uri="/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Evaluation mining/All the PDFs- Ghana/Projects/1- Evaluation.pdf"),language="en")

whatever <- purpose.in.life(paste(dir,files[2,],sep=""),targets)
whatever2 <- purpose.in.life("/Users/dwalker/Desktop/6_food_nutrition.pdf",targets)
files_sub <- data.frame(files[1:10,])

goober <- data.frame()
for(k in seq(nrow(files_sub)))
{
  pdffer <- purpose.in.life(paste(dir,files[k,],sep=""),targets)
  goober <- rbind(goober,pdffer)
}

purpose.in.life <- function(pdf,finder)
{
  PDF_pre <- readPDF(control=list(text="-raw"))(elem=list(uri=pdf),language="en")
  bomba <- data.frame()
  PDF <- str_split(PDF_pre[[1]], "\\s+")
  PDF <- unlist(PDF)
  PDF <- str_replace_all(PDF, "[[:digit:]]", "")
  PDF <- str_replace_all(PDF, "[^[:alnum:]]", "")
  PDF <- tolower(PDF)
  PDF <- str_trim(PDF)
  PDF[nchar(PDF)<2] <- ""
  PDF <- PDF[PDF!=""]
  PDF <- paste(PDF,collapse=" ")
  corpus <- Corpus(VectorSource(PDF))
  bad_adj <- seq(length(levels(finder[,1]))-1)
  bad_noun <- seq(length(levels(finder[,3]))-1)
  good_adj <- seq(length(levels(finder[,2]))-1)
  good_noun <- seq(length(levels(finder[,5])))
  for(c in 2:5)
  {
    tokenizer <- function(x) NGramTokenizer(x, Weka_control(min=c,max=c))
    options(mc.cores=1)
    tdm <- TermDocumentMatrix(corpus, control=list(tokenize = tokenizer))
    PDFb <- tdm$dimnames$Terms
    # First we get rid of the bad stuff
    for(y in bad_adj)
    {
      for(yy in bad_noun)
      {
        bad_thing <- paste(finder[y,1],finder[yy,3])
        find.it <- match(PDFb,bad_thing)
        find.it[is.na(find.it)] <- 0
        if(max(find.it)>=1)
        {
          found.it <- data.frame(project=PDF_pre$meta$id,outcome=bad_thing)
          bomba <- rbind(bomba,found.it)
        }
      }
    }
    # Then we increase the good stuff
    for(g in good_adj)
    {
      for(gg in good_noun)
      {
        good_thing <- paste(finder[g,2],finder[gg,5])
        find.it <- match(PDFb,good_thing)
        find.it[is.na(find.it)] <- 0
        if(max(find.it)>=1)
        {
          found.it <- data.frame(project=PDF_pre$meta$id,outcome=good_thing)
          bomba <- rbind(bomba,found.it)
        }
      }
    }
  }
  return(bomba)
}


PDF_test <- readPDF(control=list(text="-raw"))(elem=list(uri="/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Evaluation mining/Global Financial Inclusion 2011_evaluation.pdf"),language="en")



# First, we look for positive/ negative sentiment lexicons. There are several including -- refer to the following for a list: http://sentiment.christopherpotts.net/lexicons.html#resources
# Note that we'll want to consider context-specific words (perhaps for each project individually) and can use WordNet (https://wordnet.princeton.edu/) to expand the list of terms
# I will likely be using a modified list of SentiWords or SentiWordsNet
# For this first algorithm, however, I will be using a reduced 6000-word list


mydoc <- scan("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/OPP1128427_DevelopmentGateway_Proposal_Budget_Narrative_Version 3_June2015.txt", what="character")
wpos <- scan("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Evaluation mining/opinion-lexicon-English/positive-words.txt", what="character", comment.char=";")
wneg <- scan("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Evaluation mining/opinion-lexicon-English/negative-words.txt", what="character", comment.char=";")

# "the.feels" will score a single PDF document -- we can then loop this for multiple documents

the.feels <- function(pdf,pos,neg)
{
  PDF_a <- readPDF(control=list(text="-raw"))(elem=list(uri=pdf),language="en")
  PDF <- PDF_a[[1]]
  POS <- pos
  NEG <- neg
  for(g in c("PDF","POS","NEG"))
  {
    i <- get(g)
    i <- str_replace_all(i, "[[:digit:]]", "")
    i <- str_replace_all(i, "[^[:alnum:]]", " ")
    i <- tolower(i)
    zombie <- VCorpus(VectorSource(i))
    zombie <- tm_map(zombie, removeWords, stopwords("english"))
    zombie <- tm_map(zombie, stemDocument, language="english")
    i <- data.frame(text=unlist(sapply(zombie, `[`, "content")), stringsAsFactors=FALSE)
    i <- str_trim(i[,1])
    i[nchar(i)<2] <- ""
    i <- i[i!=""]
    assign(g,i)
  }
  word.list <- str_split(PDF, "\\s+")
  words <- unlist(word.list)
  pos.matches <- sum(!is.na(match(words, POS)))
  neg.matches <- sum(!is.na(match(words, NEG)))
  score = ((pos.matches-neg.matches)+(pos.matches+neg.matches))/((pos.matches+neg.matches)*2)*5
  return(score)
}

PDF1 <- "/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Evaluation mining/Commercial Training_evaluation.pdf"
PDF2 <- "/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Evaluation mining/Global Financial Inclusion 2011_evaluation.pdf"
PDF3 <- "/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Evaluation mining/Microenterprise Growth and the Flypaper Effect_evaluation.pdf"
test1 <- the.feels(PDF1,wpos,wneg)
test2 <- the.feels(PDF2,wpos,wneg)
test3 <- the.feels(PDF3,wpos,wneg)

# Now without stemming

the.feels2 <- function(pdf,pos,neg)
{
  PDF_a <- readPDF(control=list(text="-raw"))(elem=list(uri=pdf),language="en")
  PDF <- PDF_a[[1]]
  POS <- pos
  NEG <- neg
  for(g in c("PDF","POS","NEG"))
  {
    i <- get(g)
    i <- str_replace_all(i, "[[:digit:]]", "")
    i <- str_replace_all(i, "[^[:alnum:]]", " ")
    i <- tolower(i)
    i <- str_trim(i)
    i[nchar(i)<2] <- ""
    i <- i[i!=""]
    assign(g,i)
  }
  word.list <- str_split(PDF, "\\s+")
  words <- unlist(word.list)
  pos.matches <- sum(!is.na(match(words, POS)))
  neg.matches <- sum(!is.na(match(words, NEG)))
  score = ((pos.matches-neg.matches)+(pos.matches+neg.matches))/((pos.matches+neg.matches)*2)*5
  return(score)
}

test1.2 <- the.feels2(PDF1,wpos,wneg)
test2.2 <- the.feels2(PDF2,wpos,wneg)
test3.2 <- the.feels2(PDF3,wpos,wneg)

goober <- data.frame()
for(k in seq(nrow(files_sub)))
{
  pdffer <- the.feels2(paste(dir,files[k,],sep=""),wpos,wneg)
  goober <- rbind(goober,pdffer)
}

files_sub <- data.frame(files[1:50,])
testers <- the.feels2(paste(dir,files[101,],sep=""),wpos,wneg)

# Should try with the -1,1 list and see how results compare

rated <- read.dta13("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Evaluation mining/senti_edited2 (skinny).dta")
