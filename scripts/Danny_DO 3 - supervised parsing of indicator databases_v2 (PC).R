# GATES health data indicator project

# I'm going to write some basic code to parse the indicator datasets into subcategories

library(XLConnect)
library(stringdist)
library(tm)
library(SnowballC)
codes <- readWorksheet(loadWorkbook("C:/Users/HP/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/META_ALL_v4.2.xlsx"),sheet=1)
raw_DHS <- readWorksheet(loadWorkbook("C:/Users/HP/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/META_ALL_v4.2.xlsx"),sheet=2)
raw_GLF <- readWorksheet(loadWorkbook("C:/Users/HP/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/META_ALL_v4.2.xlsx"),sheet=3)
raw_WHO <- readWorksheet(loadWorkbook("C:/Users/HP/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/META_ALL_v4.2.xlsx"),sheet=4)
raw_WB <- readWorksheet(loadWorkbook("C:/Users/HP/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/META_ALL_v4.2.xlsx"),sheet=5)

raw_DHS <- raw_DHS[!duplicated(raw_DHS[,2]),]
raw_GLF <- raw_GLF[!duplicated(raw_GLF[,2]),]
raw_WB <- raw_WB[!duplicated(raw_WB[,2]),]
raw_WHO <- raw_WHO[!duplicated(raw_WHO[,2]),]
super_set <- data.frame(label=raw_DHS[,2],org="DHS")
super_set <- rbind(super_set,data.frame(label=raw_GLF[,2],org="GLF"))
super_set <- rbind(super_set,data.frame(label=raw_WHO[,2],org="WHO"))
super_set <- rbind(super_set,data.frame(label=raw_WB[,2],org="WB"))

whatev <- VCorpus(VectorSource(super_set[,1]))
whatev2 <- tm_map(whatev, content_transformer(tolower))
whatev2 <- tm_map(whatev2, removeWords, stopwords("english"))
whatev2 <- tm_map(whatev2, removePunctuation)
whatev2 <- tm_map(whatev2, removeNumbers)
whatev2 <- tm_map(whatev2, stemDocument, language="english")
dtm <- TermDocumentMatrix(whatev2)
dtm_deux <- as.matrix(dtm)
dtm_deux2 <- sort(rowSums(dtm_deux),decreasing=TRUE)
word_count <- data.frame(word=names(dtm_deux2),freq=dtm_deux2)
top_fifty <- word_count[1:50,]
orgs <- barplot(table(super_set[,2]))
text(orgs, 80, table(super_set[,2]),cex=1,pos=3)

all_the_datas_please <- function(x)
{
  newer <- data.frame(label=NA,org=NA,code=NA,cat=NA)
  x_l <- as.data.frame(tolower(x$label))
  for(i in seq(nrow(codes)))
  {
    for(k in seq(nrow(x)))
    {
      if(grepl(codes[i,1],x_l[k,1])==TRUE)
      {
        newer <- rbind(newer,data.frame(label=x[k,1],org=x[k,2],code=codes[i,1],cat=codes[i,2]))
      }
    }
  }
  return(na.omit(newer))
}

all_the_datas <- all_the_datas_please(super_set)
all_matern <- all_the_datas[all_the_datas$cat=="Maternal",]
all_hiv <- all_the_datas[all_the_datas$cat=="HIV",]
all_dis <- all_the_datas[all_the_datas$cat=="Disease",]
all_care <- all_the_datas[all_the_datas$cat=="Healthcare",]
all_demo <- all_the_datas[all_the_datas$cat=="Demographic",]
all_subs <- all_the_datas[all_the_datas$cat=="Substance",]

match_maker <- function(x)
{
  amber <- data.frame(orig=NA,org1=NA,match=NA,org2=NA,dist=NA)
  x_men <- 1:nrow(x)
  for(i in x_men)
  {
    y_men <- i:nrow(x)
    jennie <- data.frame(orig=x[i,1],org1=x[i,2],match=NA,org2=NA,dist=NA)
    for(k in y_men)
    {
      louis <- data.frame(orig=x[i,1],org1=x[i,2],match=x[k,1],org2=x[k,2],dist=NA)
      if(paste(louis[1,2])!=paste(louis[1,4]))
      {
        louis[1,5] <- stringdist(louis[1,1],louis[1,3])
        jennie <- rbind(jennie, louis)
      }
    }
    dennis <- jennie[order(jennie$dist),]
    dennis <- dennis[1:3,]
    amber <- rbind(amber,dennis)
  }
  return(na.omit(amber[order(amber$dist),]))
}
matchers <- match_maker(all_hiv)
matchers_pro <- match_maker(all_subs)
matchers_ultimate <- match_maker(super_set)

write.csv(top_fifty,file="/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/top_fifty.csv",row.names=FALSE)
write.csv(all_the_datas, file = "/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/META_output_v1.csv",row.names=FALSE)

write.csv(matchers_ultimate, file="C:/Users/HP/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/META_output_ultimate1.xlsx", row.names=FALSE)
