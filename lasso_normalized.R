rm(list = ls())
set.seed(7)
options(warn = -1)
setwd("../Source_Codes")
library(tree)
library(rpart)
library(e1071)
library(caret)
library(knitr)
library(Matrix)
library(class)
library(tm)
library(glmnet)
library(e1071)
library(caret)
library(SparseM)
library(randomForest)
library(FactoMineR)
library(h2o)
library(ROCR)

##########################################
# Read in the training and testing dataset
##########################################
trainingData <- foreign::read.arff("../NSL-KDD/KDDFullTrainDataBinaryClass.arff")
testingData <- foreign::read.arff("../NSL-KDD/KDDTest+.arff")

# splitSample <- sample(1:3, size=nrow(trainingData), prob=c(0.6,0.20,0.20), replace = TRUE)
# trainingSet <- trainingData[splitSample==1,]
# validationSet <- trainingData[splitSample==2,]
# testingSet <- trainingData[splitSample==3,]

dataSplit <- 0.60
singleSplit <- createDataPartition(trainingData$status, p=dataSplit, times=1, list=FALSE)
cvControl <- trainControl(method="repeatedcv", number=10, repeats=5)
trainingSet <- trainingData[singleSplit,]
testingSet <- trainingData[-singleSplit,]

  ##########################################
  ### METHOD-2 #####
  ##########################################
  cat("Lasso Binary Classification based on Method-2 \n")
  
  protocol_typeTraining=model.matrix( ~ protocol_type - 1, data=trainingSet)
  trainingSet$protocol_type=protocol_typeTraining
  serviceTraining=model.matrix( ~ service - 1, data=trainingSet)
  trainingSet$service=serviceTraining
  flagTraining=model.matrix( ~ flag - 1, data=trainingSet)
  trainingSet$flag=flagTraining
  
  protocol_typeTesting=model.matrix( ~ protocol_type - 1, data=testingSet)
  testingSet$protocol_type=protocol_typeTesting
  serviceTesting=model.matrix( ~ service - 1, data=testingSet)
  testingSet$service=serviceTesting
  flagTesting=model.matrix( ~ flag - 1, data=testingSet)
  testingSet$flag=flagTesting
  

##########################################
## remove zero variance predictors for example is_host_login, num_outbound_cmds
# nearZeroVar(trainingData) - print features with low (or zero) variances
##########################################

  #########
  # Normalize the matrix
  #########
  # get a vector of variables to drop
  zeroVarianceFeatures <- sapply(trainingSet, function(i) {  
    if((is.numeric(i) & !any(is.nan(i)) & sd(i) > 0) | is.factor(i) | is.character(i)) TRUE
    else FALSE
  })
  # subset test dropping columns that don't fit the criteria
  smallerdf <- testingSet[, zeroVarianceFeatures]
  #smallerdf2 <- smallerdf[,-c(1,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,30,31,36,37,41)]
  
  scaledDataSet <- as.data.frame(sapply(smallerdf, function(i) if(is.numeric(i)) scale(i) else i))
  scaledDataSet$status <- as.factor(scaledDataSet$status)
  #colMeans( scaledDataSet[ , ! colnames(scaledDataSet) %in% c("status") ])
  
  
  scaledDataSet[c("duration")]<-NULL
  scaledDataSet[c("land")]<-NULL
  scaledDataSet[c("urgent")]<-NULL
  scaledDataSet[c("hot")]<-NULL
  scaledDataSet[c("num_failed_logins")]<-NULL
  scaledDataSet[c("logged_in")]<-NULL
  scaledDataSet[c("num_compromised")]<-NULL
  scaledDataSet[c("root_shell")]<-NULL
  scaledDataSet[c("su_attempted")]<-NULL
  scaledDataSet[c("num_root")]<-NULL
  scaledDataSet[c("num_file_creations")]<-NULL
  scaledDataSet[c("num_shells")]<-NULL
  scaledDataSet[c("num_access_files")]<-NULL
  scaledDataSet[c("num_outbounds_cmds")]<-NULL
  scaledDataSet[c("is_host_login")]<-NULL
  scaledDataSet[c("is_guest_login")]<-NULL
  scaledDataSet[c("srv_count")]<-NULL
  scaledDataSet[c("diff_srv_rate")]<-NULL
  scaledDataSet[c("srv_diff_host_rate")]<-NULL
  scaledDataSet[c("dst_host_same_src_port_rate")]<-NULL
  scaledDataSet[c("dst_host_srv_diff_host_rate")]<-NULL
  scaledDataSet[c("dst_host_srv_rerror_rate")] <- NULL
  scaledDataSet[c("service.servicehttp_2784")] <- NULL  
  scaledDataSet[c("service.servicehttp_8001")] <- NULL
  scaledDataSet[c("service.servicered_i")] <- NULL
  scaledDataSet[c("service.servicepm_dump")] <- NULL
  scaledDataSet[c("service.serviceharvest")] <- NULL
  scaledDataSet[c("service.servicetftp_u")] <- NULL
  
  scaledDataSet[c("service.servicetim_i")] <- NULL
  scaledDataSet[c("flag.flagOTH")] <- NULL
  
  #### Let's check columns with missing values
  sapply(scaledDataSet, function(x)all(is.na(x)))
  
  naValuesTest <-  function (x) {
    w <- sapply(x, function(x)all(is.na(x)))
    if (any(w)) {
      stop(paste("All NA values are found in columns", paste(which(w), collapse=", ")))
    }
  }
  
  ## cheeck and print them out
  naValuesTest(scaledDataSet) ## columns 23 and 63 are with missing values
  
  
  
  
  
  ### Check whether a variable is a factor or not
  checkFactor<-sapply(scaledDataSet,function(x)is.factor(x))
  
  ### Get the data frame of factor variables only
  dataFrame_factorVariables<-scaledDataSet[,names(which(checkFactor=="TRUE"))]
  
  ## Find the number of levels of factor variables, if this is one we need to drop that
  ifelse(n<-sapply(dataFrame_factorVariables,function(x)length(levels(x)))==1,"DROP","NODROP")
  
###########
# apply PCA
###########
# Scale all the features,  ncp: number of dimensions kept in the results (default: 5)
pcaSelection <- PCA(scaledDataSet[,-c(100,104)], scale.unit=TRUE, ncp=5, graph=T)

# Sort the variables linked to each PC
dimdesc(pcaSelection)

################
## Apply RFE feature selection
################
# define the control
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
rfeResults <- rfe(scaledDataSet[,1:92], scaledDataSet[,93], sizes=c(1:92), rfeControl=control)
# summarize the results
print(rfeResults)
# list the chosen features
predictors(rfeResults)
# plot the results
plot(rfeResults, type=c("g", "o"))

#############
#### Feature Ranking using LVQ
############
controlRanking <- trainControl(method="repeatedcv", number=10, repeats=3)
modelRanking <- train(status~., data=scaledDataSet, method="lvq", preProcess="scale", trControl=controlRanking)
# estimate variable importance
featureImportanceRanking <- varImp(modelRanking, scale=FALSE)
print(featureImportanceRanking) # Summary of feature importance
# plot the importance of the selected features
plot(featureImportanceRanking)

##########################################
## remove zero variance predictors for example is_host_login, num_outbound_cmds
# nearZeroVar(trainingData) - print features with low (or zero) variances
##########################################
x <- model.matrix(as.factor(status)~. -1, data=scaledDataSet)
y=as.factor(scaledDataSet$status)

#########
# Normalize the matrix
#########
x <- sweep(x, 2, colSums(x), FUN="/")
x <- scale(x, center=FALSE, scale=colSums(x))
#fullMatrix=normalize_x %*% diag(1/colSums(normalize_x))  # the full matrix
x <- x[, colSums(is.na(x)) != nrow(x)] # remove any NAs
#############################
## Lasso Regression-model: fit a lasso model on the training dataset
#############################
# standardize = whether the predictors should be standardized or not
lassoModel = glmnet(x, y, 
                    family = "binomial", 
                    standardize=TRUE, alpha = 1)

#plot(lassoModel, xvar = "lambda", label = TRUE) # checking the coefficients

#############################
## Cross-validation
#############################
cvModel <- cv.glmnet(x, y,
                     family="binomial", standardize=TRUE, alpha=1,nfolds=10,type.measure="mse")
# type.measure = loss to use for cross-validation
#type.measure="auc" #ROC
#type.measure="class" #Misclassification Error


plot(cvModel)
title("Binomial Classification", line=2.7)
coef(cvModel, s = "lambda.1se")
lambdaMin = cvModel$lambda.min # Find lambda that gives the minimum cross-valiation error



#mean((lasso.pred- testingData$status)^2) 
minindex = which.min(abs(lassoModel$lambda - cvModel$lambda.min))
coef(cvModel, s = "lambda.1se")

# Non-zero coefficients
nonZeroCoefficients = coef(cvModel, s = "lambda.min")[which(coef(cvModel, s = "lambda.min") != 0)]

# Features with non-zero coefficients
nonZeroFetures <- as.data.frame(colnames(trainingSet)[which(coef(cvModel, s = "lambda.min") != 0)])

#########################################################
## The folloing function prints features and their non-zero coefficients
#########################################################

print_glmnetCoefficients <- function(cvModel, s="lambda.min") {
  index <- which(coef(cvModel, s=s) != 0)
  dataFrame <- data.frame(
    feature=rownames(coef(cvModel, s=s))[index],
    coeficient=coef(cvModel, s=s)[index]
  )
  kable(dataFrame)  # kable function from the package knitr
}

#############################################################
cat("Lambda-min: Lambda that gives minimum cross-validation error\n", lambdaMin, "\n")
cat("Lambda-minindex:\n",minindex,"\n")
cat("Lambda-min:\n",lassoModel$lambda[minindex],"\n")
cat("log(Lambda-min):\n",log(lassoModel$lambda[minindex]),"\n")

BetaValues = lassoModel$beta[,minindex]
cat("Total Number of BetaValues:\n",length(BetaValues),"\n")
cat("Total Number of non-zero BetaValues:\n",length(BetaValues[BetaValues!=0]),"\n")

BetaValues_10 = lassoModel$beta[,10]
cat("Number of non-zero BetaValues_10:\n",length(BetaValues_10[BetaValues_10!=0]),"\n")

BetaValues_25 = lassoModel$beta[,25]
cat("Number of non-zero BetaValues_25:\n",length(BetaValues_25[BetaValues_25!=0]),"\n")

BetaValues_30 = lassoModel$beta[,30]
cat("Number of non-zero BetaValues_30:\n",length(BetaValues_30[BetaValues_30!=0]),"\n")

BetaValues_35 = lassoModel$beta[,35]
cat("Number of non-zero BetaValues_35:\n",length(BetaValues_35[BetaValues_35!=0]),"\n")

BetaValues_40 = lassoModel$beta[,40]
cat("Number of non-zero BetaValues_40:\n",length(BetaValues_40[BetaValues_40!=0]),"\n")

BetaValues_50 = lassoModel$beta[,50]
cat("Number of non-zero BetaValues_50:\n",length(BetaValues_50[BetaValues_50!=0]),"\n")

BetaValues_100 = lassoModel$beta[,100]
cat("Number of non-zero BetaValues_100:\n",length(BetaValues_100[BetaValues_100!=0]),"\n")

#############################################################
#################### Confusion Matrix - RandomForest #######################
#############################################################
h2o.init(nthreads = -1)
trainHex <- as.h2o(trainingData)
testHex  <- as.h2o(testingData)
featureNames  <- colnames(testHex)
rfModel   <- h2o.randomForest(x = featureNames[!(featureNames == "status")],
                              y = "status",
                              training_frame = trainHex,
                              max_depth = 9,
                              ntrees = 500
)
rfModel
rfPrediction  <- as.data.frame(h2o.predict(rfModel, newdata = testHex))
rfConfusionMatrix <- confusionMatrix(testingData$status, rfPrediction$predict)
# print(rfConfusionMatrix)

#############################################################
#################### GLM #######################
#############################################################
glmModel   <- h2o.glm(x = featureNames[!(featureNames == "status")],
                      y = "status",
                      training_frame = trainHex,
                      family="binomial",  standardize=TRUE, alpha = 1
)
glmModel
glmPrediction  <- as.data.frame(h2o.predict(glmModel, newdata = testHex))
glmConfusionMatrix <- confusionMatrix(testingData$status, glmPrediction$predict)

#############################################################
#################### Performance Metrics #######################
#############################################################
performanceMetrics <- h2o.performance(model = glmModel, data = trainHex)
meanSquaredError <- h2o.mse(performanceMetrics)

# ### Accuracy and Cutoff
# accuracyPlot <- performance(pred,measure = "acc")
# plot(accuracyPlot,colorize=FALSE, col="blue")
# lines(c(0,1),c(0,1),col = "gray", lty = 4 )
# title("Binomial Classification", line=1.0)
# 
# ##### the maximum accuracy and the cut off
# maximumAccuracy = which.max( slot(accuracyPlot, "y.values")[[1]] )
# acc = slot(accuracyPlot, "y.values")[[1]][maximumAccuracy]
# cutoff = slot(accuracyPlot, "x.values")[[1]][maximumAccuracy]
# print(c(accuracy= acc, Cutoff = cutoff))




