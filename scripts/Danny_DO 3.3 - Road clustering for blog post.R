# Location/ objective scraping and output clustering

# Let's start with clustering

library(stringdist)
library(tm)
library(fpc)
library(SnowballC)
library(topicmodels)
library(RTextTools)
library(ggplot2)
library(stringr)
library(XLConnect)
library(cluster)
library(lava)

# First we pull in the outputs and organize

options(stringsAsFactors = FALSE)
all_outputs <- read.csv("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Deliverables/Clustering/outputs_only.csv",na.strings="",stringsAsFactors=FALSE)
all_outputs <- all_outputs[1:492,]

TOTAL_mat <- data.frame()
for(j in seq(1,nrow(all_outputs),3))
{
  lander <- as.numeric(table(!is.na(all_outputs[j,]))[2])
  numbs <- j+1
  pander <- as.numeric(table(!is.na(all_outputs[numbs,7:ncol(all_outputs[numbs,])]))[2])
  small_mat <- data.frame(donor=matrix(all_outputs[numbs,2],lander,1),id=matrix(all_outputs[numbs,1],lander,1))
  out_mat <- data.frame(t(all_outputs[j,7:(lander+6)]))
  colnames(out_mat) <- "name"
  out_numb <- data.frame(t(all_outputs[numbs,7:(lander+6)]))
  if(nrow(out_numb)<lander)
  {
    diffi <- lander-pander
    blank_mat <- matrix(NA,diffi,1)
    out_numb <- rbind(out_numb,blank_mat)
  }
  colnames(out_numb) <- "value"
  BIG_mat <- cbind(small_mat, out_mat)
  BIG_mat <- cbind(BIG_mat,out_numb)
  year <- matrix(trim(strsplit(all_outputs[numbs,4],"-")[[1]][2]),nrow(BIG_mat),1)
  BIG_mat <- cbind(BIG_mat,year)
  TOTAL_mat <- rbind(TOTAL_mat,BIG_mat)
}
row.names(TOTAL_mat) <- 1:nrow(TOTAL_mat)

# Now let's do some text cleaning

TOTAL_mat[,3] <- str_replace_all(TOTAL_mat[,3],"Outcome_","OUTOUT")
TOTAL_mat[,3] <- str_replace_all(TOTAL_mat[,3],"[[:punct:]\\s]+"," ")
outties <- grep("OUTOUT",TOTAL_mat[,3])
TOTAL_new <- TOTAL_mat[-outties,]

zombie <- VCorpus(VectorSource(TOTAL_new[,3]))
zombie_clean <- tm_map(zombie, removePunctuation)
zombie_clean <- tm_map(zombie_clean, stemDocument, language="english")
zombie_clean <- tm_map(zombie_clean, removeWords, stopwords("english"))
zombie_clean <- tm_map(zombie_clean, content_transformer(tolower))
zombie_clean <- tm_map(zombie_clean, stripWhitespace)
TOTAL_edited <- data.frame(orig=TOTAL_new[,3],output=unlist(sapply(zombie_clean, `[`, "content")),value=TOTAL_new[,4],org=TOTAL_new[,1],year=TOTAL_new[,5],stringsAsFactors=FALSE)
rm(BIG_mat,out_mat,out_numb,small_mat,TOTAL_mat,TOTAL_new,year)
TOTAL_edited[is.na(TOTAL_edited[,5]),5] <- 2016

# Now let's create a basic search function
# This will include words (clusters) and subwords (subclusters)

search_me <- function(data,word,subword)
{
  subber <- data.frame(data[grep(word,data[,2]),])
  subber_2 <- data.frame(subber[grep(subword,subber[,2]),])
  subber_2 <- rbind(subber_2,data.frame(orig="TOTAL",output="TOTAL",value=sum(as.numeric(subber_2[,3])),org="NA",year="NA"))
  return(subber_2)
}
road_rehab <- search_me(TOTAL_edited,"rehabilit","road")
road_km <- search_me(TOTAL_edited,"road","km")
farmer_train <- search_me(TOTAL_edited,"train","farmer")

road_rehab <- road_rehab[-grep("effort",road_rehab[,2]),]
road_rehab <- road_rehab[-grep("beneficiari",road_rehab[,2]),]
road_km <- road_km[-grep("maintain",road_km[,2]),]
road_km <- road_km[-grep("identifi",road_km[,2]),]
road_km <- road_km[-grep("studi",road_km[,2]),]
road_km <- road_km[grep("feeder|access|town",road_km[,2]),]
sum(as.numeric(road_km[road_km[,1]!="TOTAL",3]))
pie(c(sum(as.numeric(road_km[road_km[,4]=="IFAD",3])),sum(as.numeric(road_km[road_km[,4]=="WB",3]))),labels=c(paste("IFAD",'\n','(',sum(as.numeric(road_km[road_km[,4]=="IFAD",3])),')'),paste("WB",'\n','(',sum(as.numeric(road_km[road_km[,4]=="WB",3])),')')),clockwise=TRUE)

# Just roads since 2010
sum(as.numeric(road_km[road_km[,5]>=2008&road_km[,5]<=2012,3]))
