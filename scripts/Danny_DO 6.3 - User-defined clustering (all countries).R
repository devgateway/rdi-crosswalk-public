# User QC is required at several steps throughout this process

library(stringdist)
library(tm)
library(SnowballC)
library(RTextTools)
library(stringr)
library(XLConnect)
library(cluster)
library(jsonlite)
library(lava)

# First we pull in the outputs and organize

options(stringsAsFactors = FALSE)
OUT_GHA <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Deliverables/UTI_QCed_all countries.xlsx"),sheet=1,header=TRUE)
OUT_TNZ <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Deliverables/UTI_QCed_all countries.xlsx"),sheet=2,header=TRUE)
OUT_SRI <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Deliverables/UTI_QCed_all countries.xlsx"),sheet=3,header=TRUE)

gator <- function(x)
{
  TOTAL_mat <- data.frame()
  for(j in seq(1,nrow(x),3))
  {
    lander <- as.numeric(table(!is.na(x[j,]))[2])
    numbs <- j+1
    pander <- as.numeric(table(!is.na(x[numbs,7:ncol(x[numbs,])]))[2])
    if(is.na(pander)){next}
    small_mat <- data.frame(donor=matrix(x[numbs,2],lander,1),id=matrix(x[numbs,1],lander,1))
    out_mat <- data.frame(t(x[j,7:(lander+6)]))
    colnames(out_mat) <- "name"
    out_numb <- data.frame(t(x[numbs,7:(lander+6)]))
    if(nrow(out_numb)<lander)
    {
      diffi <- lander-pander
      blank_mat <- matrix(NA,diffi,1)
      out_numb <- rbind(out_numb,blank_mat)
    }
    colnames(out_numb) <- "value"
    BIG_mat <- cbind(small_mat, out_mat)
    BIG_mat <- cbind(BIG_mat,out_numb)
    year <- matrix(trim(strsplit(x[numbs,4],"-")[[1]][2]),nrow(BIG_mat),1)
    BIG_mat <- cbind(BIG_mat,year)
    TOTAL_mat <- rbind(TOTAL_mat,BIG_mat)
  }
  row.names(TOTAL_mat) <- 1:nrow(TOTAL_mat)
  TOTAL_mat[,3] <- tolower(TOTAL_mat[,3])
  TOTAL_mat[,3] <- trim(TOTAL_mat[,3])
  TOTAL_mat[is.na(TOTAL_mat[,5]),5] <- 2000
  TOTAL_mat[TOTAL_mat[,5]>2016,5] <- 2016
  return(TOTAL_mat)
}
big_gha <- gator(OUT_GHA)
big_sri <- gator(OUT_SRI)
big_tnz <- gator(OUT_TNZ)

# First we'll get rid of certain prefixes: "outcome," "budget," and "efforts"

gator2 <- function(y)
{
  y <- y[-grep("outcome_",y[,3]),]
  y <- y[-grep("budget_",y[,3]),]
  y <- y[-grep("efforts to ",y[,3]),]
  y[,3] <- str_replace_all(y[,3],"[[:punct:]\\s]+"," ")
  y <- na.omit(y)
}
newer_gha <- gator2(big_gha)
newer_sri <- gator2(big_sri)
newer_tnz <- gator2(big_tnz)
write.csv(newer_gha,"/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Deliverables/Clustering/new_gha_toclass3.csv")
write.csv(newer_sri,"/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Deliverables/Clustering/new_sri_toclass3.csv")
write.csv(newer_tnz,"/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Deliverables/Clustering/new_tnz_toclass3.csv")

# Note: I'm going to bypass the TDM "classing" activity from last time and instead just classify each output individually
# Note: We can use these classified datasets for future machine learning
# Note: All aggregations are done in Stata (see "Aggregating output data.do" file)

cl_ALL <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Deliverables/Clustering/classed_all countries6.xlsx"),sheet=7,header=TRUE)

# From there, we run the dataframe through the "JSON translator"

datasets2 <- data.frame(countrys=c("GHA","SRI","TNZ"))
datasets <- data.frame(sets=c("A","H"))
part0 <- paste()
for(m in seq(nrow(datasets2)))
{
  down_clust <- cl_ALL[cl_ALL[,12]==datasets2[m,1],]
  for(y in seq(nrow(datasets)))
  {
    clust_aghlth <- down_clust[down_clust[,10]==datasets[y,1]|down_clust[,10]=="B",]
    g_un <- data.frame(unique(clust_aghlth[,1]))
    part1 <- paste()
    for(w in seq(nrow(g_un)))
    {
      sub_clust <- clust_aghlth[clust_aghlth[,1]==g_un[w,1],]
      j_un <- data.frame(unique(sub_clust[,2]))
      part1 <- paste(part1,'\n{\n"name": ','"',g_un[w,1],'",\n"children": [',sep="")
      for(v in seq(nrow(j_un)))
      {
        sub_sub_clust <- sub_clust[sub_clust[,2]==j_un[v,1],]
        m_un <- data.frame(unique(sub_sub_clust[,3]))
        part1 <- paste(part1,'\n{\n"name": ','"',j_un[v,1],'",\n"children": [',sep="")
        for(l in seq(nrow(m_un)))
        {
          sub_sub_sub_clust <- sub_sub_clust[sub_sub_clust[,3]==m_un[l,1],]
          h_un <- data.frame(unique(sub_sub_sub_clust[,5]))
          part1 <- paste(part1,'\n{\n"name": ','"',m_un[l,1],'",\n"children": [',sep="")
          for(q in seq(nrow(h_un)))
          {
            sub_4_clust <- sub_sub_sub_clust[sub_sub_sub_clust[,5]==h_un[q,1],]
            s_un <- data.frame(unique(sub_4_clust[,6:11]))
            part1 <- paste(part1,'\n{\n"name": ','"',h_un[q,1],'",\n"years": [',sep="")
            for(p in seq(nrow(s_un)))
            {
              if(p==nrow(s_un)){part1 <- paste(part1,'\n{\n"year": ',s_un[p,6],',\n"size": ',s_un[p,1],'\n}',sep="")}
              else{part1 <- paste(part1,'\n{\n"year": ',s_un[p,6],',\n"size": ',s_un[p,1],'\n},',sep="")}
            }
            if(q==nrow(h_un)){part1 <- paste(part1,'\n]\n}',sep="")}
            else{part1 <- paste(part1,'\n]\n},',sep="")}
          }
          if(l==nrow(m_un)){part1 <- paste(part1,'\n]\n}',sep="")}
          else{part1 <- paste(part1,'\n]\n},',sep="")}
        }
        if(v==nrow(j_un)){part1 <- paste(part1,'\n]\n}',sep="")}
        else{part1 <- paste(part1,'\n]\n},',sep="")}
      }
      if(w==nrow(g_un)){part1 <- paste(part1,'\n]\n}',sep="")}
      else{part1 <- paste(part1,'\n]\n},',sep="")}
    }
    part0 <- paste('{\n"name": "Total goods and services delivered",\n"children": [',part1,'\n]\n}',sep="")
    writeLines(part0,paste("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Deliverables/Clustering/",datasets2[m,1],"_",datasets[y,1],"_json1.txt",sep=""),sep="\n")
  }
}

# We can use this tool to verify that the output is JSON: http://jsonlint.com/
