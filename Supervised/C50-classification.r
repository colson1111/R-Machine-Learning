# Using C5.0 Classification
# http://connor-johnson.com/2014/08/29/decision-trees-in-r-using-the-c50-package/
# http://static1.squarespace.com/static/51156277e4b0b8b2ffe11c00/t/51e67b45e4b0e6c130fb4d54/1374059333633/user_C5.0.pdf

# How is C5.0 different from CART?
#  1. C5.0 uses entropy as its impurity measure
#  2. Tree pruning is done using pessimistic pruning
#  3. Splits on categorical predictors are handled differently
#  4. Trees can be converted to rules.
#  5. Missing values are handled by snding fractional samples into subsquent nodes
library(C50)
library(caret)
library(e1071)

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data"
crx <- read.table( file=url, header=FALSE, sep="," )

# randomize rows
crx <- crx[ sample( nrow( crx ) ), ]

# create feature matrix (X) and target vector (y)
X <- crx[,1:15]
y <- crx[,16]

# Create training and test data
trainX <- X[1:600,]
trainy <- y[1:600]
testX <- X[601:690,]
testy <- y[601:690]


# build model: single decision tree
model <- C5.0(trainX, trainy)
summary(model)

# build model:  boosting
model2 <- C5.0(trainX, trainy, trials = 10)
summary(model2)


# Prediction
p <- predict(model, testX, type = "class")
sum(p == testy) / length(p)

p2 <- predict(model2, testX, type = "class")
sum(p2 == testy) / length(p2)


# Using caret to find optimal model type, winnowing, and boosting
tune <- caret::train(trainX, trainy, method = "C5.0", tuneLength = 11,
              trControl = caret::trainControl(method = "repeatedcv", repeats = 5))

# best model:  model = tree, winnow = false, boosting trials = 60
postResample(predict(tune, testX), testy)

plot(tune, metric = "Kappa")
