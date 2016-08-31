# This provides a new search function for micro-level data

library(foreign)
library(readstata13)
library(XLConnect)
library(stringdist)
library(stringr)
library(tm)
library(SnowballC)
library(RTextTools)
library(sp)
library(reshape)

# Need to figure out what the units of observation is and how to incorporate that into the code
# This speaks to the larger point of how to incorporate spatial information into the code
# I might consider adding a database of lower-level administrative units for each country
# You're going to have to go through each dataset by itself anyway -- let's just go through one and see all the information we'd need to record -- i.e. which variables to keep, what type of variables they are (baseline or output), what category of variable they are (categorical, ones that should be averaged, ones that should be aggregated, etc.), what the unit of observation is, etc.
# I'll start with one in Ghana just as soon as this stata code is finished running, which will probably be never

super_set_maker <- function(x)
{
  dta.files <- data.frame(set=dir(x, pattern =".dta"))
  sav.files <- data.frame(set=dir(x, pattern =".sav"))
  csv.files <- data.frame(set=dir(x, pattern =".csv"))
  super_set <- data.frame()
  error_docs <- data.frame()
  # First we import Stata files (.dta)
  if(nrow(dta.files)>0){
  for(i in seq(nrow(dta.files)))
  {
    namer <- paste(x,"/",dta.files[i,],sep="")
    demo <- data.frame(strsplit(paste(dta.files[i,]),"_"))
    stata_ver <- try(read.dta(namer,convert.factors=FALSE),silent=TRUE)
    if(is(stata_ver,"try-error"))
    {
      stata_new_ver <- try(read.dta13(namer,convert.factors=FALSE),silent=TRUE)
      if(is(stata_new_ver,"try-error"))
      {
        error_note <- data.frame(set=dta.files[i,],type="DTA")
        error_docs <- rbind(error_docs,error_note)
        next
      }
      else{opener <- read.dta13(namer,convert.factors=FALSE)}
    }
    else{opener <- read.dta(namer,convert.factors=FALSE)}
    arranger <- data.frame(vnames=colnames(opener),labels=attr(opener,"var.labels"),country=demo[1,],org=demo[2,],year=demo[3,],set=demo[4,],origs=attr(opener,"var.labels"))
    db1 <- arranger[!duplicated(arranger[,2]),]
    db1[,2] <- str_replace_all(db1[,2], "[[:digit:]]", "")
    db1[,2] <- str_replace_all(db1[,2], "[^[:alnum:]]", " ")
    db1$vnames <- tolower(db1$vnames)
    db1$labels <- tolower(db1$labels)
    zombie <- VCorpus(VectorSource(db1[,2]))
    zombie <- tm_map(zombie, removeWords, stopwords("english"))
    zombie <- tm_map(zombie, stemDocument, language="english")
    db1[,2] <- data.frame(text=unlist(sapply(zombie, `[`, "content")), stringsAsFactors=FALSE)
    db1$labels <- str_trim(db1$labels)
    db1[nchar(db1$labels)<3,] <- NA
    db1 <- na.omit(db1)
    super_set <- rbind(super_set,db1)
  }}
  # Now we import SPSS files (.sav)
  if(nrow(sav.files)>0){
  for(j in seq(nrow(sav.files)))
  {
    namer <- paste(x,"/",sav.files[j,],sep="")
    demo <- data.frame(strsplit(paste(sav.files[j,]),"_"))
    spss_ver <- try(read.spss(namer,to.data.frame=TRUE),silent=TRUE)
    if(is(spss_ver,"try-error"))
    {
      error_note <- data.frame(set=sav.files[j,],type="SAV")
      error_docs <- rbind(error_docs,error_note)
      next
    }
    else{opener <- read.spss(namer,to.data.frame=TRUE)}
    arranger <- data.frame(vnames=colnames(opener),labels=attr(opener,"variable.labels"),country=demo[1,],org=demo[2,],year=demo[3,],set=demo[4,],origs=attr(opener,"variable.labels"))
    db1 <- arranger[!duplicated(arranger[,2]),]
    db1[,2] <- str_replace_all(db1[,2], "[[:digit:]]", "")
    db1[,2] <- str_replace_all(db1[,2], "[^[:alnum:]]", " ")
    db1$vnames <- tolower(db1$vnames)
    db1$labels <- tolower(db1$labels)
    zombie <- VCorpus(VectorSource(db1[,2]))
    zombie <- tm_map(zombie, removeWords, stopwords("english"))
    zombie <- tm_map(zombie, stemDocument, language="english")
    db1[,2] <- data.frame(text=unlist(sapply(zombie, `[`, "content")), stringsAsFactors=FALSE)
    db1$labels <- str_trim(db1$labels)
    db1[nchar(db1$labels)<3,] <- NA
    db1 <- na.omit(db1)
    super_set <- rbind(super_set,db1)
  }}
  # Now we import Excel files (.csv)
  if(nrow(csv.files)>0){
  for(k in seq(nrow(csv.files)))
  {
    namer <- paste(x,"/",csv.files[k,],sep="")
    demo <- data.frame(strsplit(paste(csv.files[k,]),"_"))
    excel_ver <- try(read.csv(namer,to.data.frame=TRUE),silent=TRUE)
    if(is(excel_ver,"try-error"))
    {
      error_note <- data.frame(set=csv.files[k,],type="CSV")
      error_docs <- rbind(error_docs,error_note)
      next
    }
    else{opener <- read.csv(namer,to.data.frame=TRUE)}
    arranger <- data.frame(vnames=colnames(opener),labels=attr(opener,"variable.labels"),country=demo[1,],org=demo[2,],year=demo[3,],set=demo[4,],origs=attr(opener,"variable.labels"))
    db1 <- arranger[!duplicated(arranger[,2]),]
    db1[,2] <- str_replace_all(db1[,2], "[[:digit:]]", "")
    db1[,2] <- str_replace_all(db1[,2], "[^[:alnum:]]", " ")
    db1$vnames <- tolower(db1$vnames)
    db1$labels <- tolower(db1$labels)
    zombie <- VCorpus(VectorSource(db1[,2]))
    zombie <- tm_map(zombie, removeWords, stopwords("english"))
    zombie <- tm_map(zombie, stemDocument, language="english")
    db1[,2] <- data.frame(text=unlist(sapply(zombie, `[`, "content")), stringsAsFactors=FALSE)
    db1$labels <- str_trim(db1$labels)
    db1[nchar(db1$labels)<3,] <- NA
    db1 <- na.omit(db1)
    super_set <- rbind(super_set,db1)
  }}
  composite <- paste(super_set[,2],super_set[,3],super_set[,4],super_set[,5],super_set[,6],sep=",")
  super_set <- super_set[!duplicated(composite),]
  super_set$dups <- NA
  new_composite <- paste(super_set[,2],super_set[,4],super_set[,6],sep=",")
  super_set[duplicated(new_composite,fromLast=TRUE),8] <- "yes"
  super_set <- super_set[!duplicated(new_composite),]
  output_lister <- list(set=super_set,errors=error_docs)
  return(output_lister)
}

n_wd <- "/Users/dwalker/Desktop/IPSO (local)/Crosswalk test data (subset)"
my_precious <- super_set_maker(n_wd)
super_set <- my_precious[[1]]
error_docs <- my_precious[[2]]

codes_list <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/Themes (top 100 pull) v2.2.xlsx"),sheet=2,header=FALSE)
zombie <- VCorpus(VectorSource(codes_list[,1]))
zombie2 <- tm_map(zombie, stemDocument, language="english")
stem_codes <- codes_list
stem_codes$stem <- data.frame(text=unlist(sapply(zombie2, `[`, "content")), stringsAsFactors=FALSE)

split_and_tag <- function(x)
{
  x$tags <- NA
  for(i in seq(nrow(x)))
  {
    mini <- data.frame(strsplit(paste0(x[i,2])," "))
    if(nrow(mini)<1){next}
    for(q in seq(nrow(mini)))
    {
      if(nchar(paste(mini[q,1]))<1){mini[q,1] <- NA}
    }
    mini <- na.omit(mini)
    if(nrow(mini)<1){next}
    mini$taggit <- NA
    for(w in seq(nrow(mini)))
    {
      for(e in seq(nrow(stem_codes)))
      {
        if(paste0(mini[w,1])==paste0(stem_codes[e,3])){mini$taggit[w] <- stem_codes[e,2]}
      }
    }
    mini <- na.omit(mini)
    mini_2 <- data.frame(table(mini[,2]))
    if(nrow(mini_2)<1){next}
    mini_2 <- mini_2[order(mini_2[,2],decreasing=TRUE),][1:3,]
    if(is.na(mini_2[2,1])){mini_2[2,1] <- mini_2[1,1]}
    if(is.na(mini_2[3,1])){mini_2[3,1] <- mini_2[1,1]}
    mini_2 <- mini_2[order(mini_2[,1]),]
    x$tags[i] <- paste(mini_2[,1],collapse=" - ")
  }
  x <- cbind(x,colsplit(x$tags,split=" - ",names=c("tag1","tag2","tag3")))
  return(x)
}

system.time(super_set_tags <- split_and_tag(super_set))
super_set_tags_nomiss <- super_set_tags[!is.na(super_set_tags$tags),]
# system times: 75/1000, 477/5000, 1040/10000, 2630/18696
# nomiss: 320/1000, 1743/5000, 2658/10000, 5109/18696

write.csv(super_set_tags,"/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/super_set_tags_v1.csv")
write.csv(super_set_tags_nomiss,"/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/super_set_tags_nomiss_v1.csv")

# ***
super_set_tags_v1 <- read.csv("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/super_set_tags_v1.csv")
super_set_tags <- super_set_tags_v1[,2:10]
super_set_tags_nomiss <- super_set_tags[!is.na(super_set_tags$tags),]
# ***

super_test <- super_set_tags_nomiss[super_set_tags_nomiss$tags=="Maternal - Maternal - Maternal",]
super_test2 <- super_set_tags_nomiss[super_set_tags_nomiss$tags=="Consumption and drugs - Consumption and drugs - Consumption and drugs",]

share_and_compare <- function(x)
{
  x$idfier <- paste(x[,3],x[,4],x[,5],x[,6],sep=",")
  groupie <- data.frame(unique(x[,9]))
  group_matcher <- data.frame()
  deez_da_same <- data.frame()
  for(z in seq(nrow(groupie)))
  {
    carrigor <- paste(groupie[z,1])
    subsetter <- x[x[,9]==carrigor,]
    checker <- data.frame(unique(subsetter$idfier))
    if(nrow(checker)<2)
    {
      print(paste("This category --",carrigor,"-- only appears in one set"))
      next
    }
    new_setter <- data.frame()
    same_size <- data.frame()
    sub_sub1 <- seq(nrow(subsetter))
    for(b in sub_sub1)
    {
      sub_sub2 <- b:nrow(subsetter)
      sweet <- data.frame()
      for(j in sub_sub2)
      {
        bitter <- data.frame(orig=subsetter[b,7],set1=subsetter[b,10],match=subsetter[j,7],set2=subsetter[j,10],dist=NA)
        if(paste(bitter[,2])==paste(bitter[,4])){next}
        bitter[1,5] <- stringdist(subsetter[b,2],subsetter[j,2],method="jw",p=0.1)
        sweet <- rbind(sweet,bitter)
      }
      if(nrow(sweet)<1){next}
      savor <- sweet[order(sweet[,5]),]
      flavor_flav <- savor[savor[,5]<0.01,]
      savor2 <- savor[savor[,5]>0.01,]
      savor2 <- savor2[1,]
      new_setter <- rbind(new_setter,savor2)
      same_size <- rbind(same_size,flavor_flav)
    }
    group_matcher <- rbind(group_matcher,new_setter)
    group_matcher <- group_matcher[order(group_matcher[,5]),]
    deez_da_same <- rbind(deez_da_same,same_size)
  }
  da_outputs <- list(matched=group_matcher,twins=deez_da_same)
  return(da_outputs)
}

more_output <- share_and_compare(super_test)
more_output2 <- share_and_compare(super_test2)
ultra_set <- more_output[[1]]
ultra_set2 <- more_output2[[1]]
ultra_same <- more_output[[2]]
ultra_same2 <- more_output2[[2]]
ultra_set_sims <- ultra_set[ultra_set$dist<0.24,]
ultra_set_sims2 <- ultra_set2[ultra_set2$dist<0.24,]

write.csv(ultra_set_sims,"/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/ultra_set_sims.csv")
write.csv(ultra_set_sims2,"/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/ultra_set_sims2.csv")

ultra_set_EDITED <- read.csv("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/ultra_set_sims (edited).csv")
ultra_set_EDITED2 <- read.csv("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/ultra_set_sims2 (edited).csv")

# First, I need to update the aggregation code so that each indicator can be matched to multiple other indicators
  # I'll need to add in a piece where duplicate indicator pairs are dropped
  # Also, I'll just leave in the same-name indicators (i.e. jw==0)
# Next, I need to re-mark the actual links (human intervention part)
# Then I need to figure out a way to merge columns based on the marked indicator pairs (or groups)
# Next we can figure out the "most recent year" stuff

the_great_aggregator <- function(x)
{
  
}