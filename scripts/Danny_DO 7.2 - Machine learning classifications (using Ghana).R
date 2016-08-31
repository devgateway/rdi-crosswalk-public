# Now we're going to reclassify the Ghana data and use it for machine learning

library(stringdist)
library(tm)
library(SnowballC)
library(RTextTools)
library(stringr)
library(XLConnect)
library(cluster)
library(jsonlite)
library(lava)

# Note: there is a bug in the "create_matrix" code that will throw an error if you try to use tf/idf weights
# To fix: trace("create_matrix",edit=TRUE)
# change line 42 from "Acronym" to "acronym"

# First we load in the data:

TRAINER <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Deliverables/Clustering/classed_ghana_dexm1.xlsx"),sheet=1,header=TRUE)

# Note: Tanzania data is loaded in twice -- this is so we can assess the accuracy of the different machine learning methods.
# Note: I'll also be testing the classification processes both with and without the "NA" category

### WITHOUT NA -- TRAINING AND TESTING ###
# First we make a training matrix and container and train the models

test_TNZ <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Deliverables/Clustering/classed_ghana_dexm1.xlsx"),sheet=7,header=TRUE)
train_mat <- create_matrix(TRAINER[,3], language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTfIdf)
contain_1 <- create_container(train_mat,TRAINER[,4],trainSize=1:nrow(TRAINER),virgin=FALSE)
models_1 <- train_models(contain_1, algorithms=c("SVM","MAXENT","BOOSTING","BAGGING","RF","NNET"))

# Next we'll creat the testing matrix and container -- note: the container had a matrix of zero's -- rep(0,nrow(test_TNZ)) -- because these are meant to be filled in using the trained models

test_TNZ_mat <- create_matrix(test_TNZ[,2], language="english", originalMatrix=train_mat, removeNumbers=TRUE, stemWords=TRUE, weighting=weightTfIdf)
contain_TNZ <- create_container(test_TNZ_mat,rep(0,nrow(test_TNZ)),testSize=1:nrow(test_TNZ), virgin=FALSE)

# Now we classify the outputs based on the trained models (note: the "create_analytics" function is more for visualization purposes)

results_TNZ <- classify_models(contain_TNZ, models_1)
analytics_TNZ <- create_analytics(contain_TNZ, results_TNZ)

# Now let's paste in the results and assess accuracy

coded_TNZ <- cbind(test_TNZ,results_TNZ)
sum(coded_TNZ[,3]==coded_TNZ[,6], na.rm=TRUE)/nrow(coded_TNZ)
sum(coded_TNZ[,3]==coded_TNZ[,8], na.rm=TRUE)/nrow(coded_TNZ)
sum(coded_TNZ[,3]==coded_TNZ[,10], na.rm=TRUE)/nrow(coded_TNZ)
sum(coded_TNZ[,3]==coded_TNZ[,12], na.rm=TRUE)/nrow(coded_TNZ)
sum(coded_TNZ[,3]==coded_TNZ[,14], na.rm=TRUE)/nrow(coded_TNZ)
sum(coded_TNZ[,3]==coded_TNZ[,16], na.rm=TRUE)/nrow(coded_TNZ)

# It looks like the SVM and MAXENT methods are consistently the most accurate

### WITH NA -- TRAINING AND TESTING ###
TRAINER_NA <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Deliverables/Clustering/classed_ghana_dexm1.xlsx"),sheet=1,header=TRUE)
test_TNZ <- readWorksheet(loadWorkbook("/Users/dwalker/Desktop/Dropbox/1 - IPSO BUMBLES/Gates/Deliverables/Clustering/classed_ghana_dexm1.xlsx"),sheet=7,header=TRUE)
