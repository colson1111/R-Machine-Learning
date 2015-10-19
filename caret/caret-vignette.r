#  Learning the caret package
# https://cran.r-project.org/web/packages/caret/vignettes/caret.pdf

# ensure caret and all its dependencies are installed.  They will be loaded as needed.
# install.packages("caret", dependencies = c("Depends", "Suggests"))

library(caret)
library(mlbench)

data(Sonar)
set.seed(107)

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
