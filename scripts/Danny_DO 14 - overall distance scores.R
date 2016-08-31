# Ok, now I want to loop the function so that I get an overall distance score for each indicator
# It is important to include minimum distances of indicators between organizations such that we can see the most relatable indicators used by ALL organizations instead of just the most relatable indicators in the whole database
# Then I'm going to look at the distribution of overall distance scores to see breaks -- I'd like to have 4 or 5

# You could also create an overall score for each organization and visualize the distributions of each

# Basically I'm just going to loop the function and record stats from it

library(XLConnect)
library(stringdist)
library(stringr)
library(tm)
library(plyr)
library(SnowballC)

pr_ind <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/RDI old data sources/Donor priority indicator lists/pr__ALL DONORS.xlsx"),sheet=1,header=TRUE)
pr_ind_sm <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/RDI old data sources/Donor priority indicator lists/pr__ALL DONORS_sm.xlsx"),sheet=1,header=TRUE)

codes_list <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/RDI old data sources/Donor priority indicator lists/Keywords list v2.2.xlsx"),sheet=2,header=FALSE)
zombie <- VCorpus(VectorSource(codes_list[,1]))
zombie2 <- tm_map(zombie, stemDocument, language="english")
stem_codes <- codes_list
stem_codes$stem <- data.frame(text=unlist(sapply(zombie2, `[`, "content")), stringsAsFactors=FALSE)

everythings_jw <- function(x)
{
  big_reffer <- data.frame(org=unique(x[,2:3]))
  big_reffer <- cbind(data.frame(num=seq(nrow(big_reffer))),big_reffer)
  end_paster <- data.frame(row.names=c("low","med","high","v.high","over"))
  for(v in big_reffer[,1])
  {
    datas <- split_pull_match_play(x,big_reffer[v,2],big_reffer[v,3])
    humvie <- subset(datas,select=grep(".dist",names(datas)))
    billfold <- cbind(datas[,1],data.frame(total=rowMeans(humvie,na.rm=TRUE)))
    sub_paster <- data.frame(row.names=c("low","med","high","v.high","over"))
    sub_paster[1,1] <- sum(billfold[,2]<0.2)
    sub_paster[2,1] <- sum(billfold[,2]>0.2&billfold[,2]<0.4)
    sub_paster[3,1] <- sum(billfold[,2]>0.4&billfold[,2]<0.6)
    sub_paster[4,1] <- sum(billfold[,2]>0.6)
    sub_paster[5,1] <- mean(billfold[,2])
    names(sub_paster) <- big_reffer[v,2]
    end_paster <- cbind(end_paster,sub_paster)
  }
  return(end_paster)
}

everythings_dl <- function(x)
{
  big_reffer <- data.frame(org=unique(x[,2:3]))
  big_reffer <- cbind(data.frame(num=seq(nrow(big_reffer))),big_reffer)
  end_paster <- data.frame(row.names=c("low","med","high","v.high","over"))
  for(v in big_reffer[,1])
  {
    datas <- split_pull_match_play(x,big_reffer[v,2],big_reffer[v,3])
    humvie <- subset(datas,select=grep(".dist",names(datas)))
    billfold <- cbind(datas[,1],data.frame(total=rowMeans(humvie,na.rm=TRUE)))
    sub_paster <- data.frame(row.names=c("low","med","high","v.high","over"))
    sub_paster[1,1] <- sum(billfold[,2]<30)
    sub_paster[2,1] <- sum(billfold[,2]>29&billfold[,2]<40)
    sub_paster[3,1] <- sum(billfold[,2]>39&billfold[,2]<50)
    sub_paster[4,1] <- sum(billfold[,2]>49)
    sub_paster[5,1] <- mean(billfold[,2])
    names(sub_paster) <- big_reffer[v,2]
    end_paster <- cbind(end_paster,sub_paster)
  }
  return(end_paster)
}

all_the_things3 <- everythings_jw(pr_ind)
all_the_things4 <- everythings_dl(pr_ind_sm)

write.csv(all_the_things4, file="/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/RDI old data sources/Donor priority indicator lists/all_donors_dl groups_sm.csv", row.names=FALSE)

# We should add in a list of our own "stopwords" such as "rate" or "per"