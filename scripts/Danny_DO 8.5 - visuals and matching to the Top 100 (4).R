# Matching to the Top 100

library(stringdist)
library(XLConnect)
library(tm)
library(SnowballC)
library(topicmodels)
library(RTextTools)
library(graph)
library(Rgraphviz)
library(plyr)
library(RWeka)
library(VennDiagram)
library(ggplot2)

# Note: there is a bug in the "create_matrix" code that will throw an error if you try to use tf/idf weights
# To fix: trace("create_matrix",edit=TRUE)
# change line 42 from "Acronym" to "acronym"

####################################################
########## FIGURE 2: Association of terms ##########
####################################################

raw_DHS <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/RDI old data sources/META_ALL_v4.2.xlsx"),sheet=2)
raw_GLF <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/RDI old data sources/META_ALL_v4.2.xlsx"),sheet=3)
raw_WHO <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/RDI old data sources/META_ALL_v4.2.xlsx"),sheet=4)
raw_WB <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/RDI old data sources/META_ALL_v4.2.xlsx"),sheet=5)

raw_DHS <- raw_DHS[!duplicated(raw_DHS[,2]),]
raw_GLF <- raw_GLF[!duplicated(raw_GLF[,2]),]
raw_WB <- raw_WB[!duplicated(raw_WB[,2]),]
raw_WHO <- raw_WHO[!duplicated(raw_WHO[,2]),]
super_set <- data.frame(label=raw_DHS[,2],org="DHS")
super_set <- rbind(super_set,data.frame(label=raw_GLF[,2],org="GLF"))
super_set <- rbind(super_set,data.frame(label=raw_WHO[,2],org="WHO"))
super_set <- rbind(super_set,data.frame(label=raw_WB[,2],org="WB"))

matrix_super_idf <- create_matrix(super_set[,1], language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTfIdf)
freq_super_idf <- findFreqTerms(matrix_super_idf,lowfreq=70)
plot(matrix_super_idf,term=freq_super_idf,corThreshold=0.08,weighting=TRUE,attrs=list(node=list(fontsize=40,height=10,width=10),edge=list(color="lightblue")))

matrix_super_tf <- create_matrix(super_set[,1], language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTf)
freq_super_tf <- findFreqTerms(matrix_super_tf,lowfreq=70)
plot(matrix_super_tf,term=freq_super_idf,corThreshold=0.08,weighting=TRUE,attrs=list(node=list(fontsize=40,height=10,width=10),edge=list(color="lightblue")))

##############################################
########## TABLE 2: LDA predictions ##########
##############################################

row_totes_super <- apply(matrix_super_tf, 1, sum)
matrix_super_tf <- matrix_super_tf[row_totes_super > 0,]
lda_super_tf <- LDA(matrix_super_tf, 20)
terms(lda_super_tf)
table(topics(lda_super_tf))

##########################################################
########## FIGURE 3: Association of terms (new) ##########
##########################################################

raw_UNI <- read.csv("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/META_DISCO_v2.5.csv")
raw_UNI <- raw_UNI[!duplicated(raw_UNI[,2]),]

matrix_UNI_idf <- create_matrix(cbind(as.vector(raw_UNI[,2]),as.vector(raw_UNI[,3])), language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTfIdf)
freq_UNI_idf <- findFreqTerms(matrix_UNI_idf,lowfreq=4)
plot(matrix_UNI_idf,term=freq_UNI_idf,corThreshold=0.19,weighting=TRUE,attrs=list(node=list(fontsize=40,height=10,width=10),edge=list(color="lightblue")))

matrix_UNI_tf <- create_matrix(cbind(as.vector(raw_UNI[,2]),as.vector(raw_UNI[,3])), language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTf)
freq_UNI_tf <- findFreqTerms(matrix_UNI_tf,lowfreq=150)
plot(matrix_UNI_tf,term=freq_UNI_idf,corThreshold=0.19,weighting=TRUE,attrs=list(node=list(fontsize=40,height=10,width=10),edge=list(color="lightblue")))

####################################################
########## TABLE 3: LDA predictions (new) ##########
####################################################

row_totes_UNI <- apply(matrix_UNI_tf, 1, sum)
matrix_UNI_tf <- matrix_UNI_tf[row_totes_UNI > 0,]
lda_UNI_tf <- LDA(matrix_UNI_tf, 10)
terms(lda_UNI_tf)
table(topics(lda_UNI_tf))

#######################################################################
########## TABLE 4: WHO classified using the Top 100 (small) ##########
#######################################################################

raw_100 <- read.csv("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/Top 100_v2.csv")
raw_100$code1 <- as.numeric(as.factor(raw_100[,2]))
raw_100$code2 <- as.numeric(as.factor(raw_100[,3]))
rownames(raw_WHO) <- 1:nrow(raw_WHO)
raw_100_codes <- ddply(raw_100, .(domain, tags, code1, code2), function(x) head(x,1))
keep <- c("domain","code1","tags","code2")
raw_100_codes <- raw_100_codes[keep]

matrix_100_small <- create_matrix(raw_100[,1], language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTfIdf)
contain_100_small <- create_container(matrix_100_small,raw_100$code2,trainSize=1:nrow(raw_100),virgin=FALSE)
models_100_small <- train_models(contain_100_small, algorithms=c("SVM","MAXENT","BOOSTING","BAGGING","RF","NNET","TREE"))

raw_WHO$codes <- 0
matrix_WHO_small <- create_matrix(raw_WHO[,2], language="english", originalMatrix=matrix_100_small, removeNumbers=TRUE, stemWords=TRUE)
contain_WHO_small <- create_container(matrix_WHO_small,raw_WHO$codes,testSize=1:nrow(raw_WHO), virgin=FALSE)
results_WHO_small <- classify_models(contain_WHO_small, models_100_small)
analytics_WHO_small <- create_analytics(contain_WHO_small, results_WHO_small)
coded_WHO_small <- cbind(raw_WHO,results_WHO_small)

samp_rows <- c(57, 1, 5, 60, 59, 1304, 1306, 1305, 1115, 2, 82, 947, 1292, 1009)
samp_cols <- c(2, 5, 7, 9, 11, 13, 15, 17)
who_samp <- coded_WHO_small[samp_rows,samp_cols]

enter_da_codes <- function(x)
{
  x_men <- 2:ncol(x)
  for(i in seq(nrow(x)))
  {
    for(w in x_men)
    {
      x[,w] <- as.character(x[,w])
      for(q in seq(nrow(raw_100_codes)))
      {
        if(x[i,w]==raw_100_codes[q,4]){x[i,w] <- paste(raw_100_codes[q,3])}
      }
    }
  }
  return(x)
}
who_samp <- enter_da_codes(who_samp)

write.csv(who_samp,"/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/who_samp.csv",row.names=FALSE)

######################################################################
########## TABLE 5: WHO classified using the Top 100 (full) ##########
######################################################################

matrix_100_full <- create_matrix(cbind(as.vector(raw_100[,1]),as.vector(raw_100[,4])), language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTfIdf)
contain_100_full <- create_container(matrix_100_full,raw_100$code2,trainSize=1:nrow(raw_100),virgin=FALSE)
models_100_full <- train_models(contain_100_full, algorithms=c("SVM","MAXENT","BOOSTING","BAGGING","RF","NNET","TREE"))

matrix_WHO_full <- create_matrix(raw_WHO[,2], language="english", originalMatrix=matrix_100_full, removeNumbers=TRUE, stemWords=TRUE)
contain_WHO_full <- create_container(matrix_WHO_full,raw_WHO$codes,testSize=1:nrow(raw_WHO), virgin=FALSE)
results_WHO_full <- classify_models(contain_WHO_full, models_100_full)
analytics_WHO_full <- create_analytics(contain_WHO_full, results_WHO_full)
coded_WHO_full <- cbind(raw_WHO,results_WHO_full)

who_samp2 <- coded_WHO_full[samp_rows,samp_cols]
who_samp2 <- enter_da_codes(who_samp2)

write.csv(who_samp2,"/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/who_samp2.csv",row.names=FALSE)

################################################################################
########## TABLE 6: WHO/ UNICEF classified using the Top 100 (fuller) ##########
################################################################################

rownames(raw_UNI) <- 1:nrow(raw_UNI)

models_100_fuller <- train_models(contain_100_full, algorithms=c("MAXENT","BOOSTING","BAGGING","RF"))

raw_UNI$codes <- 0
matrix_UNI_full <- create_matrix(cbind(as.vector(raw_UNI[,2]),as.vector(raw_UNI[,3])), language="english", originalMatrix = matrix_100_full, removeNumbers=TRUE, stemWords=TRUE, weighting=weightTfIdf)
contain_UNI_full <- create_container(matrix_UNI_full,raw_UNI$codes,testSize=1:nrow(raw_UNI), virgin=FALSE)
results_UNI_full <- classify_models(contain_UNI_full, models_100_fuller)
analytics_UNI_full <- create_analytics(contain_UNI_full, results_UNI_full)
coded_UNI_full <- cbind(raw_UNI,results_UNI_full)

samp_rows2 <- c(4, 9, 71, 284, 285, 294)
samp_cols2 <- c(2, 6, 8, 10, 12)
uni_samp <- coded_UNI_full[samp_rows2,samp_cols2]
uni_samp <- enter_da_codes(uni_samp)

write.csv(uni_samp,"/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/uni_samp.csv",row.names=FALSE)

############################################################################
########## TABLE 7: WHO/ UNICEF classified - Top 100 (TF weights) ##########
############################################################################

matrix_100_TF <- create_matrix(cbind(as.vector(raw_100[,1]),as.vector(raw_100[,4])), language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTf)
contain_100_TF <- create_container(matrix_100_TF,raw_100$code2,trainSize=1:nrow(raw_100),virgin=FALSE)
models_100_TF <- train_models(contain_100_TF, algorithms=c("MAXENT","BOOSTING","BAGGING","RF"))

matrix_UNI_TF <- create_matrix(cbind(as.vector(raw_UNI[,2]),as.vector(raw_UNI[,3])), language="english", originalMatrix = matrix_100_TF, removeNumbers=TRUE, stemWords=TRUE, weighting=weightTf)
contain_UNI_TF <- create_container(matrix_UNI_TF,raw_UNI$codes,testSize=1:nrow(raw_UNI), virgin=FALSE)
results_UNI_TF <- classify_models(contain_UNI_TF, models_100_TF)
analytics_UNI_TF <- create_analytics(contain_UNI_TF, results_UNI_TF)
coded_UNI_TF <- cbind(raw_UNI,results_UNI_TF)

uni_samp_tf <- coded_UNI_TF[samp_rows2,samp_cols2]
uni_samp_tf <- enter_da_codes(uni_samp_tf)

write.csv(uni_samp_tf,"/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Health data sources and indicators/uni_samp_tf2.csv",row.names=FALSE)

################################################################################
########## TABLE 8/ FIGURE 4: Comparison of IDF/ TF and meta/ no meta ##########
################################################################################

matrix_100_small_tf <- create_matrix(raw_100[,1], language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTf)

mat_1 <- sort(colSums(as.matrix(matrix_100_small)),decreasing=TRUE)
mat_2 <- sort(colSums(as.matrix(matrix_100_small_tf)),decreasing=TRUE)
mat_3 <- sort(colSums(as.matrix(matrix_100_full)),decreasing=TRUE)
mat_4 <- sort(colSums(as.matrix(matrix_100_TF)),decreasing=TRUE)

words_1 <- data.frame(word=names(mat_1),freq=mat_1,row.names=c(1:length(mat_1)))[1:100,]
words_2 <- data.frame(word=names(mat_2),freq=mat_2,row.names=c(1:length(mat_2)))[1:100,]
words_3 <- data.frame(word=names(mat_3),freq=mat_3,row.names=c(1:length(mat_3)))[1:100,]
words_4 <- data.frame(word=names(mat_4),freq=mat_4,row.names=c(1:length(mat_4)))[1:100,]
big_compare <- data.frame(IDF=words_1[,1],TF=words_2[,1],IDF_met=words_3[,1],TF_met=words_4[,1])
big_lists <- list(IDF=words_1[,1],TF=words_2[,1],IDF_met=words_3[,1],TF_met=words_4[,1])
vinnie <- venn.diagram(big_lists,filename = "word_freq_comparisons2.tiff",category.names=c("IDF weights,\nno metadata","TF weights,\nno metadata","IDF weights\nwith metadata","TF weights\nwith metadata"))

######################################################
########## MORE TABLES/ FIGURES/ AND TWEAKS ##########
######################################################

lda_super_tf2 <- LDA(matrix_super_tf, 10)

super_set$code <- 1
super_set[1823,3] <- 0
super_set[2867,3] <- 0
sub_super <- super_set[super_set$code==1,]

terms_super_tf <- terms(lda_super_tf2,k=5)
terms_super_tf <- as.data.frame(apply(terms_super_tf,MARGIN=2,paste,collapse=", "))
topics_super_tf <- topics(lda_super_tf,1)
topics_super_tf <- data.frame(org=sub_super[,2],topics_super_tf)
Organizations <- topics_super_tf[,1]
Themes_and_keywords <- terms_super_tf[,1][topics_super_tf[[2]]]
qplot(Organizations,..count..,geom="density",fill=Themes_and_keywords,position="stack")
