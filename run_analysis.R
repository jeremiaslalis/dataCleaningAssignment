run_analysis <- function(wd){
  mergeSubject <- function(dataSet, subject){
    cbind(dataSet, subject)
  }
  
  mergeActivity <- function(dataSet, activity){
    cbind(dataSet, activity)
  }
  
  mergeTrainTestSet <- function(train, test){
    rbind(train, test)
  }
  
  getMeanStdCol <- function(dataSet){
    dataSet[grep("mean\\b|std\\b",features$V2),1:2]
  }
  
  ## Main
  setwd(wd)
  
  ## Phase 1
  ## Phase 1.1
  trainingSet <- read.table("./train/X_train.txt")
  subject <- read.table("./train/subject_train.txt")
  activity <-  read.table("./train/y_train.txt")
  mergedTrainingSet <- mergeActivity(mergeSubject(trainingSet, subject), activity)
  
  ## Phase 1.2
  testingSet <- read.table("./test/X_test.txt")
  subject <- read.table("./test/subject_test.txt")
  activity <-  read.table("./test/y_test.txt")
  mergedTestingSet <- mergeActivity(mergeSubject(testingSet, subject), activity)
  
  ## Phase 1.3
  mergedDataSet <- mergeTrainTestSet(mergedTrainingSet, mergedTestingSet)
  
  ## Phase 2
  ## Phase 2.1
  features <- read.table("features.txt")
  meanStdIndex <- getMeanStdCol(features)
  
  ## Phase 2.2
  includedCol <- c(meanStdIndex[,1], ncol(mergedDataSet)-1, ncol(mergedDataSet))
  
  ## Phase 2.3
  filteredDataSet <- mergedDataSet[,includedCol]
  
  ## Phase 3
  filteredDataSet <- setNames(filteredDataSet,c(as.character(meanStdIndex[,2]), "subject", "activity_names"))
  
  ## Phase 4
  ## Phase 4.1
  activityLabels <-read.table("activity_labels.txt")
  activityLabels[,2] <- as.character(activityLabels[,2])
  
  ## Phase 4.2
  tidyDataSet <- filteredDataSet
  tidyDataSet$activity_names <- activityLabels[match(tidyDataSet$activity_names,activityLabels[,1]),2]
  tidyDataSet
  
  ## Phase 5
  endCol <- ncol(tidyDataSet)-2
  newTidySet <- aggregate(as.matrix(tidyDataSet[,1:endCol])~ subject + activity_names, tidyDataSet, mean)
}