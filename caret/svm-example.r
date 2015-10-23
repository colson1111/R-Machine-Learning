# http://blog.revolutionanalytics.com/2015/10/the-5th-tribe-support-vector-machines-and-caret.html

library(caret)
library(dplyr)
library(kernlab)
library(pROC)

# Get data
data(segmentationData)
trainIndex <- createDataPartition(segmentationData$Case, p = 0.5, list = FALSE)
trainData <- segmentationData[trainIndex,]
testData <- segmentationData[-trainIndex,]
trainX <- trainData[,4:61] # keep variables for training
sapply(trainX, summary)    # summary of training data

# svm model
set.seed(802)
# set up cross validation
ctrl <-  trainControl(method = "repeatedcv",             # 10 fold cross validation
                      repeats = 5,                       # 5 repetitions of cv
                      summaryFunction = twoClassSummary, # optimize to AUC
                      classProbs = TRUE)

# train and tune svm
svm.tune <- train(x = trainX,
                  y = trainData$Class,
                  method = "svmRadial",                  # Radial kernel
                  tuneLength = 9,                        # use 9 values for cost
                  preProc = c("center", "scale"),         # center and scale
                  metric = "ROC",
                  trControl = ctrl)
# optimal parameters:  sigma = 0.01531539 and C = 1

# second pass
set.seed(802)
grid <- expand.grid(sigma = c(0.01, 0.015, 0.2),
                    C = c(0.75, 0.9, 1, 1.1, 1.25))

svm.tune2 <- train(x = trainX,
                   y = trainData$Class,
                   method = "svmRadial",
                   preProc = c("center", "scale"),
                   metric = "ROC",
                   tuneGrid = grid,
                   trControl = ctrl)
# optimal parameters:  sigma = 0.01 and C = 1.25

