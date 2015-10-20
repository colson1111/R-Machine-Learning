#  Learning the caret package

# ensure caret and all its dependencies are installed.  They will be loaded as needed.
# install.packages("caret", dependencies = c("Depends", "Suggests"))

library(caret)
library(mlbench)

data(Sonar)
set.seed(802)

inTrain <- createDataPartition(y = Sonar$Class,  # The outcome data
                               p = .75,          # The percentage of data in the training set
                               list = FALSE)     # The format of the results
# Output is a set of rows in the training set

training <- Sonar[inTrain,]
testing <- Sonar[-inTrain,]
nrow(training)
nrow(testing)

# Partial Least Squares Discriminant Analysis, tuned over # of PLS components that should be retained
# Basic
plsFit <- train(Class ~ .,
                data = training,
                method = "pls",
                preProc = c("center", "scale") # center and scale the predictors for the training set and all future samples
                )

# Advanced
# 1.  Expand # of PLS models to choose from (default is 3, change to 15)
# 2.  Change to k-fold CV resampling (default is bootstrap)
# 3.  Change to use ROC as measurement of performance (default is overall accuracy and kappa statistic)

ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

plsFit <- train(Class ~ .,
                data = training,
                method = "pls",
                tuneLength = 15, # evaluate integers between 1 and 15, use tuneGrid for specific values
                trControl = ctrl,
                metric = "ROC",
                preProc = c("center", "scale") # center and scale the predictors for the training set and all future samples
)
plsFit

# plot ROC by # of pls components
plot(plsFit)

# Predict new samples
plsClasses <- predict(plsFit, newdata = testing)
str(plsClasses)

plsProbs <- predict(plsFit, newdata = testing, type = "prob")
head(plsProbs)

# Confusion Matrix
confusionMatrix(data = plsClasses, testing$Class)

# Fit a different model to the data (regularized discriminant model)
rdaGrid <- data.frame(gamma = (0:4) / 4, lambda = 3/4)
set.seed(802)
rdaFit <- train(Class ~ .,
                data = training,
                method = "rda",
                tuneGrid = rdaGrid,
                trControl = ctrl,
                metric = "ROC")
rdaFit

# Predict new data and view confusion matrix
rdaClasses <- predict(rdaFit, newdata = testing)
confusionMatrix(rdaClasses, testing$Class)


# Use resamples function to collect, summarize, and contrast resampling results
# make sure all set.seed() functions match above so the folds are the same
resamps <- resamples(list(pls = plsFit, rda = rdaFit))
summary(resamps)

# plot to visualize results
xyplot(resamps, what = "BlandAltman")

# paired t-test to assess whether there is a difference in the avg. resampled AUROC
diffs <- diff(resamps)
summary(diffs)

# difference between the models is -0.028 ROC units (RDA is higher).
# two-sided p-value is 0.2812, suggesting no significant difference between the two models.
