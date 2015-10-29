# caret pre-processing examples
# http://topepo.github.io/caret/preprocess.html

library(earth)

# CREATING DUMMY VARIABLES: dummyVars
# etitanic data has two factors:  pclass1 (3 levels) and sex (2 levels)
data(etitanic)
dummies <- dummyVars(survived ~ ., data = etitanic)
head(predict(dummies, newdata = etitanic))


# ZERO AND NEAR ZERO VARIANCE PREDICTORS: nearZeroVar
data(mdrr)
data.frame(table(mdrrDescr$nR11))

# to identify these predictors:
# Frequency Ratio: frequency of most prevalent value over the frequency of the second most prevalent value
# Percent of Unique Values:  Number of unique values / total number of samples
# if either of these values is under some threshold, it might be a zero-variance predictor
dim(mdrrDescr)  # 528 rows, 342 columns

nzv <- nearZeroVar(mdrrDescr, saveMetrics = TRUE)
nzv <- nearZeroVar(mdrrDescr)
filteredDescr <- mdrrDescr[, -nzv]
dim(filteredDescr) # 528 rows, 297 columns:  filters out 45 variables


# LINEAR DEPENDENCIES:  findLinearCombos
ltfrDesign <- matrix(0, nrow = 6, ncol = 6)
ltfrDesign[,1] <- c(1,1,1,1,1,1)
ltfrDesign[,2] <- c(1,1,1,0,0,0)
ltfrDesign[,3] <- c(0,0,0,1,1,1)
ltfrDesign[,4] <- c(1,0,0,1,0,0)
ltfrDesign[,5] <- c(0,1,0,0,1,0)
ltfrDesign[,6] <- c(0,0,1,0,0,1)

# For each linear combination of columns, it will incrementally remove columns from the matrix and
# test to see if the dependencies have been resolved.
comboInfo <- findLinearCombos(ltfrDesign)
comboInfo
ltfrDesign[, -comboInfo$remove]

# CENTERING AND SCALING: preProcess
set.seed(96)
inTrain <- sample(seq(along = mdrrClass), length(mdrrClass) / 2)

training <- filteredDescr[inTrain,]
test <- filteredDescr[-inTrain,]
trainMDRR <- mdrrClass[inTrain]
testMDRR <- mdrrClass[-inTrain]

preProcValues <- preProcess(training, method = c("center", "scale"))

trainTransformed <- predict(preProcValues, training)
testTransformed <- predict(preProcValues, test)

# IMPUTATION:  preProcess
# uses k-nearest neighbors or bagged trees to impute missing values based on other training data.

# TRANSFORMATIONS:  preProcess
# Principal Component Analysis Transformation:  method = "pca"
# Independent Component Analysis Transformation:  method = "ica"
# Spatial Sign Transformation:  spatialSign()

# Example of Spatial Sign Transformation:
library(AppliedPredictiveModeling)
transparentTheme(trans = 0.4)

plotSubset <- data.frame(scale(mdrrDescr[, c("nC", "X4v")]))

# before spatial sign transformation
xyplot(nC ~ X4v,
       data = plotSubset,
       groups = mdrrClass,
       auto.key = list(columns = 2))

# after spatial sign transformation
transformed <- spatialSign(plotSubset)
transformed <- as.data.frame(transformed)
xyplot(nC ~ X4v,
       data = transformed,
       groups = mdrrClass,
       auto.key = list(columns = 2))

# Box-Cox transformation
preProcValues2 <- preProcess(training, method = "BoxCox")
trainBC <- predict(preProcValues2, training)
testBC <- predict(preProcValues2, test)
preProcValues2



# CLASS DISTANCE CALCULATIONS:  classDist
centroids <- classDist(trainBC2, trainMDRR)
distances <- predict(centroids, testBC)
distances <- as.data.frame(distances)
head(distances)

xyplot(dist.Active ~ dist.Inactive,
       data = distances,
       groups = testMDRR,
       auto.key = list(columns = 2))


