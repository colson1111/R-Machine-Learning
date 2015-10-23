# https://www.youtube.com/watch?v=7Jbb2ItbTC4
library(C50)
library(caret)
library(foreach)
library(doSNOW)
library(pROC)

data(churn)

# view churnTrain and churnTest structure
str(churnTrain)
str(churnTest)

# variable names other than response (churn)
predictors <- names(churnTrain)[names(churnTrain) != "churn"]
# note: functions predict probability of first factor level ("yes" in this case).  We are predicting churn, not retention

# DATA SPLITTING
# create stratified training/test sets:  stratified sample maintains proportion of responses
allData <- rbind(churnTrain, churnTest)
set.seed(1)
inTrainingSet <- createDataPartition(allData$churn, p = 0.75, list = FALSE)
churnTrain <- allData[inTrainingSet,]
churnTest <- allData[-inTrainingSet,]

# DATA PRE-PROCESSING METHODS
numerics <- c("account_length", "total_day_calls", "total_night_calls")

# determine means and sd's of parameters with transformations
# Need to use correct ordering (ex. a box-cox transformation must have + data, if it occurs after 
# center/scale, it won't work)
procValues <- preProcess(churnTrain[,numerics], method = c("center", "scale", "YeoJohnson")) 
procValues

# use predict to do the adjustments on both training and test data
trainScaled <- predict(procValues, churnTrain[,numerics])
testScaled <- predict(procValues, churnTest[,numerics])

# We generally would run preprocessing within cross-validation folds


# create cluster
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

# MODEL TUNING USING TRAIN
ctrl <- trainControl(method = "repeatedcv",                # use repeated cross validation
                     repeats = 5,                          # repeat 5 times
                     classProbs = TRUE,                    # produce probability of class membership (in addition to class prediction)
                     summaryFunction = twoClassSummary)    # twoClassSummary calculates sensitivity, specificity, and ROC AUC

grid <- expand.grid(interaction.depth = seq(1, 7, by = 2), # find best value for tree depth
                    n.trees = seq(100, 1000, by = 50),     # find best value for n trees
                    shrinkage = c(0.01, 0.1),             # find best value for shrinkage parameter
                    n.minobsinnode = 1)                    # minimum terminal node size

gbmTune <- train(churn ~ .,                                # prediction formula
                 data = churnTrain,                        # use dataset churnTrain
                 method = "gbm",                           # generalized boosted models
                 metric = "ROC",                           # optimize for area under ROC curve
                 tuneGrid = grid,                          # use cross-validation to test values in grid
                 verbose = FALSE,                          # suppress printing results
                 trControl = ctrl)                         # use ctrl inputs above

stopCluster(cl)

ggplot(gbmTune) + theme(legend.position = "tmp")

# PREDICTIONS ON TEST SET
# predict class
gbmPred <- predict(gbmTune, churnTest)
str(gbmPred)

# predict class probability
gbmProbs <- predict(gbmTune, churnTest, type = "prob")
str(gbmProbs)

# confusion matrix
confusionMatrix(gbmPred, churnTest$churn)

rocCurve <- roc(response = churnTest$churn,
                predictor = gbmProbs[, "yes"],
                levels = rev(levels(churnTest$churn)))
rocCurve

plot(rocCurve,
     print.thres = c(0.5, 0.2),
     print.thres.pch = 16,
     print.thres.cex = 1.2)



# additional stuff
set.seed(1)
svmTune <- train(churn ~ ., data = churnTrain,
                 ## tell it to fit a SVM model and tune
                 ## over cost and the RBF parameter
                 method = "svmRadial",
                 ## this pre-processing will be applied to
                 ## these data and new samples too
                 preProc = c("center", "scale"),
                 tuneLength = 10,
                 trControl = ctrl,
                 metric = "ROC")

set.seed(1)
fdaTune <- train(churn ~ ., data = churnTrain,
                 ## now try a flexible discriminant model
                 ## using MARS basis functions
                 method = "fda",
                 tuneLength = 10,
                 trControl = ctrl,
                 metric = "ROC")





