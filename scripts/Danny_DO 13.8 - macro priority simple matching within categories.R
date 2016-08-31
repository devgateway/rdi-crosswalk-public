# Simple indicator comparisons by organization
# Rows: Indicators of Organization X
# Columns: Most-similar indicators of Organization Y

library(XLConnect)
library(stringdist)
library(stringr)
library(tm)
library(plyr)
library(SnowballC)

pr_ind <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/RDI old data sources/Donor priority indicator lists/pr__ALL DONORS.xlsx"),sheet=1,header=TRUE)
pr_ind_sm <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/RDI old data sources/Donor priority indicator lists/pr__ALL DONORS_sm.xlsx"),sheet=1,header=TRUE)

codes_list <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/RDI old data sources/Donor priority indicator lists/Keywords list v2.2.xlsx"),sheet=2,header=FALSE)
codes_list[] <- lapply(codes_list, as.character)
zombie <- VCorpus(VectorSource(codes_list[,1]))
zombie2 <- tm_map(zombie, stemDocument, language="english")
stem_codes <- codes_list
stem_codes$stem <- data.frame(text=unlist(sapply(zombie2, `[`, "content")), stringsAsFactors=FALSE)

############
# Note: the inputs of the following function are an organization (e.g. "WB") and a category (i.e. "h" for health and "a" for agriculture)

split_pull_match_play <- function(x,donor,ha)
{
  # First we load in the data and subset it by organization and theme
  
  subber <- x[x$cat==ha,1:2]
  ref_theme <- data.frame(a="agricultural",h="health")
  to_match <- data.frame(ind=subber[subber$org==donor,])
  to_find <- subber[subber$org!=donor,]
  if(nrow(to_match)==0)
  {
    print(paste(donor,"does not have",ref_theme[1,ha],"indicators"))
    break
  }
  print(paste(donor,"has",ref_theme[1,ha],"indicators; you're good to go!"))
  
  # Now we're going to stem and trim and remove stopwords, etc.
  
  zombie <- VCorpus(VectorSource(subber[,1]))
  zombie <- tm_map(zombie, removeWords, stopwords("english"))
  zombie <- tm_map(zombie, stemDocument, language="english")
  stemmed <- data.frame(stemmed=unlist(sapply(zombie, `[`, "content")), stringsAsFactors=FALSE)
  subber <- cbind(subber,stemmed)
  subber[,3] <- str_replace_all(subber[,3], "[[:digit:]]", "")
  subber[,3] <- str_replace_all(subber[,3], "[^[:alnum:]]", " ")
  subber[,3] <- tolower(subber[,3])
  subber[,3] <- str_trim(subber[,3])
  
  # This loop splits each indicator into individual words
  
  super_list <- list()
  for(g in seq(nrow(subber)))
  {
    listic <- list(org=subber[g,2],original=subber[g,1],stemmed=subber[g,3])
    mini <- data.frame(strsplit(paste0(subber[g,3])," "))
    if(nrow(mini)<1){next}
    for(q in seq(nrow(mini)))
    {
      if(nchar(paste(mini[q,1]))<1){mini[q,1] <- NA}
    }
    mini <- na.omit(mini)
    if(nrow(mini)<1){next}
    mini[,2] <- NA
    
    # This loop matches keywords and assigns categories
    
    for(w in seq(nrow(mini)))
    {
      for(e in seq(nrow(stem_codes)))
      {
        if(paste0(mini[w,1])==paste0(stem_codes[e,3])){mini[w,2] <- stem_codes[e,2]}
      }
    }
    mini <- na.omit(mini)
    if(nrow(mini)<1){next}
    listic$keywords <- mini[,1]
    listic$categories <- data.frame(unique(mini[,2]))
    super_list[[g]] <- listic
  }
  
  # This function classifies indicators in all applicable categories
  
  for(u in seq(super_list))
  {
    row_test <- nrow(super_list[[u]]$categories)
    if(is.null(row_test))
    {
      mini_dat_frame <- data.frame(matrix(NA,1,5))
      names(mini_dat_frame) <- c("org","original","stemmed","keywords","categories")
      super_list[[u]] <- mini_dat_frame
      next
    }
    else
    {
      mini_dat_frame <- data.frame(matrix(NA,row_test,4))
      mini_dat_frame[,1] <- super_list[[u]]$org
      mini_dat_frame[,2] <- super_list[[u]]$original
      mini_dat_frame[,3] <- super_list[[u]]$stemmed
      mini_dat_frame[,4] <- paste(super_list[[u]]$keywords,collapse=" ")
      mini_dat_frame <- cbind(mini_dat_frame, as.data.frame(super_list[[u]]$categories))
      names(mini_dat_frame) <- c("org","original","stemmed","keywords","categories")
      super_list[[u]] <- mini_dat_frame
    }
  }
  MEGA_subber <- data.frame()
  for(s in seq(length(super_list)))
  {
    MEGA_subber <- rbind(MEGA_subber,as.data.frame(super_list[[s]]))
  }
  MEGA_subber <- na.omit(MEGA_subber[,1:5])
  
  # Now we're going to cluster the indicators by category and match within clusters
  
  ref_org <- data.frame(organ=unique(to_find$org))
  ref_org <- data.frame(organ=ref_org[rev(order(ref_org[,1])),])
  ref_org <- cbind(data.frame(num=seq(1:nrow(ref_org))),ref_org)
  ref_cat <- data.frame(num=seq(unique(MEGA_subber[,5])),cat=unique(MEGA_subber[,5]))
  all_orgs_and_themes <- data.frame()
  MEGA_to_match <- MEGA_subber[MEGA_subber$org==donor,]
  MEGA_to_find <- MEGA_subber[MEGA_subber$org!=donor,]
  new_ref_themes <- data.frame(num=seq(unique(MEGA_to_match[,5])),theme=unique(MEGA_to_match[,5]))
  for(o in seq(new_ref_themes[,1]))
  {
    theme <- new_ref_themes[o,2]
    tm_cluster <- MEGA_to_match[MEGA_to_match[,5]==theme,]
    tf_cluster <- MEGA_to_find[MEGA_to_find[,5]==theme,]
    all_orgs_one_theme <- data.frame(row.names=1:nrow(tm_cluster))
    for(h in seq(ref_org[,1]))
    {
      org_within_theme <- ref_org[h,2]
      tf_sub_cluster <- tf_cluster[tf_cluster[,1]==org_within_theme,]
      one_org_one_theme <- data.frame()
      if(dim(tf_sub_cluster)[1]<1)
      {
        print(paste("Reference donor --",org_within_theme,"-- does not have indicators within the",theme,"theme. Pasting in blank matrix ..."))
        blank_matrix <- data.frame(matrix(NA,nrow(tm_cluster),2))
        blank_matrix <- cbind(tm_cluster[,2],blank_matrix)
        all_orgs_one_theme <- cbind(all_orgs_one_theme,blank_matrix)
        next
      }
      print(paste(org_within_theme,"has indicators within the",theme,"theme. Proceed, good sir or madam."))
      for(x in seq(nrow(tm_cluster)))
      {
        matches_within_theme <- data.frame()
        for(y in seq(nrow(tf_sub_cluster)))
        {
          matches_within_theme[y,1] <- tm_cluster[x,2]
          matches_within_theme[y,2] <- tf_sub_cluster[y,2]
          matches_within_theme[y,3] <- stringdist(tm_cluster[x,3],tf_sub_cluster[y,3],method="dl")
        }
        matches_within_theme <- matches_within_theme[order(matches_within_theme[,3]),]
        one_org_one_theme <- rbind(one_org_one_theme,matches_within_theme[1,])
      }
      all_orgs_one_theme <- cbind(all_orgs_one_theme,one_org_one_theme)
    }
    ref_org[,3] <- paste(ref_org[,2],".dist",sep="")
    ref_org[,1] <- "drop"
    var_names <- data.frame(row.names=1)
    for(a in seq(ref_org[,1]))
    {
      sub_namer <- ref_org[a,1:3]
      var_names <- cbind(var_names,sub_namer)
    }
    var_names[] <- lapply(var_names, as.character)
    names(all_orgs_one_theme) <- var_names[1,]
    all_orgs_and_themes <- rbind(all_orgs_and_themes,all_orgs_one_theme)
  }
  names(all_orgs_and_themes)[1] <- paste(donor,"(original)")
  all_orgs_and_themes <- subset(all_orgs_and_themes, select=-seq(4,length(all_orgs_and_themes),3))
  return(all_orgs_and_themes)
}

PEPFAR_h4 <- split_pull_match_play(pr_ind,"PEPFAR","h")
UNICEF_h4 <- split_pull_match_play(pr_ind,"UNICEF","h")

DHS_h <- split_pull_match_play(pr_ind,"DHS","h")
GHO_h <- split_pull_match_play(pr_ind,"GHO","h")
WB_h <- split_pull_match_play(pr_ind,"WB","h")

write.csv(DFID_h, file="/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/RDI old data sources/Donor priority indicator lists/DFID_h_prind.csv", row.names=FALSE)
