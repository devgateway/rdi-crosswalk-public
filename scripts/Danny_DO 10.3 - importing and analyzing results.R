# This exercise is meant to go through the whole process (for health)

# First we specify the packages we need
library(foreign)
library(readstata13)
library(XLConnect)
library(stringdist)
library(stringr)
library(tm)

# Now we import the health keyword list and stem it
codes_list <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/Themes (top 100 pull) v2.2.xlsx"),sheet=2,header=FALSE)
zombie <- VCorpus(VectorSource(codes_list[,1]))
zombie2 <- tm_map(zombie, stemDocument, language="english")
stem_codes <- codes_list
stem_codes$stem <- data.frame(text=unlist(sapply(zombie2, `[`, "content")), stringsAsFactors=FALSE)

# Now we're going to reference our database of datasets and set the directory
o_wd <- getwd()
n_wd <- "/Users/dwalker/Desktop/IPSO (local)/Crosswalk test data (RAW)"
setwd(n_wd)

############################################
##### PULLING IN RESULTS DATA (HEALTH) #####
############################################
# And now we're going to loop through and create a super set
# Note: the input for this function is your database directory

super_set_maker <- function(x)
{
  dta.files <- data.frame(set=dir(x, pattern =".dta"))
  sav.files <- data.frame(set=dir(x, pattern =".sav"))
  super_set <- data.frame()
  error_docs <- data.frame()
  # First we import Stata files (.dta)
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
    arranger <- data.frame(vnames=colnames(opener),labels=attr(opener,"var.labels"),country=demo[1,],org=demo[2,],year=demo[3,],set=demo[4,])
    db1 <- arranger[!duplicated(arranger[,2]),]
    db1[,2] <- str_replace_all(db1[,2], "[[:digit:]]", "")
    db1[,2] <- str_replace_all(db1[,2], "[^[:alnum:]]", " ")
    db1$vnames <- tolower(db1$vnames)
    db1$labels <- tolower(db1$labels)
    zombie <- VCorpus(VectorSource(db1[,2]))
    zombie <- tm_map(zombie, removeWords, stopwords("english"))
    zombie <- tm_map(zombie, stemDocument, language="english")
    db1[,2] <- data.frame(text=unlist(sapply(zombie, `[`, "content")), stringsAsFactors=FALSE)
    super_set <- rbind(super_set,db1)
  }
  # Now we import SPSS files (.sav)
  for(j in seq(nrow(sav.files)))
  {
    namer <- paste(x,"/",sav.files[j,],sep="")
    demo <- data.frame(strsplit(paste(dta.files[j,]),"_"))
    spss_ver <- try(read.spss(namer,to.data.frame=TRUE),silent=TRUE)
    if(is(spss_ver,"try-error"))
    {
      error_note <- data.frame(set=sav.files[j,],type="SAV")
      error_docs <- rbind(error_docs,error_note)
      next
    }
    else{opener <- read.spss(namer,to.data.frame=TRUE)}
    arranger <- data.frame(vnames=colnames(opener),labels=attr(opener,"variable.labels"),country=demo[1,],org=demo[2,],year=demo[3,],set=demo[4,])
    db1 <- arranger[!duplicated(arranger[,2]),]
    db1[,2] <- str_replace_all(db1[,2], "[[:digit:]]", "")
    db1[,2] <- str_replace_all(db1[,2], "[^[:alnum:]]", " ")
    db1$vnames <- tolower(db1$vnames)
    db1$labels <- tolower(db1$labels)
    zombie <- VCorpus(VectorSource(db1[,2]))
    zombie <- tm_map(zombie, removeWords, stopwords("english"))
    zombie <- tm_map(zombie, stemDocument, language="english")
    db1[,2] <- data.frame(text=unlist(sapply(zombie, `[`, "content")), stringsAsFactors=FALSE)
    super_set <- rbind(super_set,db1)
  }
  composite <- paste(super_set[,2],super_set[,3],super_set[,4],super_set[,5],super_set[,6],sep=",")
  super_set <- super_set[!duplicated(composite),]
  output_lister <- list(set=super_set,errors=error_docs)
  return(output_lister)
}

my_precious <- super_set_maker(n_wd)
super_set <- my_precious[[1]]
error_docs <- my_precious[[2]]

##########################################
##### TAGGING THE VARIABLES (HEALTH) #####
##########################################
# Now that we have the super set, we'll tag the variables
# NOTE: the "super set" has already been cleaned and stemmed

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
  return(x)
}

super_set_tags <- split_and_tag(super_set)
# TIME: about 100 seconds per 500 rows
super_set_tags_nomiss <- na.omit(super_set_tags)

##################################################
##### MATCHING THE TAGGED VARIABLES (HEALTH) #####
##################################################
# The "share_and_compare" function uses data frames created by "split_and_tag"

share_and_compare <- function(x)
{
  x$idfier <- paste(x[,3],x[,4],x[,5],x[,6],sep=",")
  groupie <- data.frame(unique(x[,7]))
  group_matcher <- data.frame()
  deez_da_same <- data.frame()
  for(z in seq(nrow(groupie)))
  {
    carrigor <- paste(groupie[z,1])
    subsetter <- x[x[,7]==carrigor,]
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
        bitter <- data.frame(orig=subsetter[b,2],set1=subsetter[b,8],match=subsetter[j,2],set2=subsetter[j,8],dist=NA)
        if(paste(bitter[,2])==paste(bitter[,4])){next}
        bitter[1,5] <- stringdist(subsetter[b,2],subsetter[j,2],method="jw",p=0)
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

more_output <- share_and_compare(super_set_tags_nomiss)
# TIME: about 400 seconds per 500 rows
ultra_set <- more_output[[1]]
ultra_same <- more_output[[2]]
ultra_set_sims <- ultra_set[ultra_set$dist<0.34,]
rownames(ultra_set_sims) <- 1:nrow(ultra_set_sims)
hist1 <- hist(ultra_set$dist, breaks=100, main="Frequencies of edit distances",xlab="Edit distance (method=jw)")
abline(v=0.34,lwd=3,col="blue")

# Now what do we do with the things...?