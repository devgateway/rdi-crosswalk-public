# Machine learning algorithm for documents
# We'll start with World Bank documents

library(tm)
dir <- "/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/RDI scripts/Machine learning/"
da.files <- data.frame(files=list.files(dir,pattern=".txt"))

end_dat <- data.frame()
for(q in seq(length(da.files[,1])))
{
  examp <- readLines(paste(dir,da.files[q,1],sep=""))
  month_list <- c("january","february","march","april","may","june","july","august","september","october","november","december")
  blob <- paste(examp,collapse=" ")
  Encoding(blob) <- "latin1"
  zombie <- VCorpus(VectorSource(blob))
  zombie_clean <- tm_map(zombie, content_transformer(tolower))
  zombie_clean <- tm_map(zombie_clean, stripWhitespace)
  new_blob <- zombie_clean[[1]]$content
  whole_thing <- data.frame(words=strsplit(new_blob," "),stringsAsFactors=FALSE)
  stopper1 <- min(grep(paste(month_list,collapse="|"),whole_thing[,1]))
  first_info <- data.frame(whole_thing[1:stopper1,1],stringsAsFactors=FALSE)
  moola <- grep("\\$",first_info[,1])
  mills <- grep("million",first_info[,1])
  budget <- data.frame()
  for(m in seq(length(moola)))
  {
    for(l in seq(length(mills)))
    {
      if((mills[l]-moola[m])<=2&&(mills[l]-moola[m])>0)
      {
        all_the_money <- data.frame(amount=paste(first_info[moola[m]:mills[l],1],collapse=" "))
        budget <- rbind(budget,all_the_money)
        break
      }
    }
  }
  budget <- data.frame(amount=gsub("\\(", "", budget[,1]))
  project <- data.frame()
  for(k in seq(nrow(first_info)))
  {
    if(first_info[k,1]=="for"&&first_info[k+1,1]=="a")
    {
      but_why <- data.frame(project_name=paste(first_info[(k+2):(nrow(first_info)-1),1],collapse=" "))
      project <- rbind(project,but_why)
    }
  }
  
}
  
  
  
  for(i in 1:length(grep(indexer,PDF_pre)))
  {
    ier <- grep(indexer,PDF_pre)[i]
    jer <- ier+1
    indic[i] <- paste(PDF_pre[ier:jer],collapse=" ")
  }
  indic2 <- indic[(substr(indic[1], 1, nchar(indexer))==indexer)==TRUE]
  full_list <- rbind(full_list,as.matrix(indic2))

edited_list <- data.frame(ind=full_list[(substr(full_list[,1],1,9)=="Indicator")==TRUE,])
edited_list <- cbind(edited_list,data.frame(org="SDG"))
edited_list <- cbind(edited_list,data.frame(cat="h"))
