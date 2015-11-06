# SVM Walkthrough and Examples
# https://rpubs.com/ryankelly/svm

library(e1071)

set.seed(1)
#Create our own test data
x <- matrix(rnorm(20*2), ncol=2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1

plot(x, col = (3 - y))

# SUPPORT VECTOR CLASSIFIER

# SVC classification with linear kernel and cost = 10
dat <- data.frame(x = x, y = as.factor(y))
svm.fit <- svm(y ~ ., data = dat, kernel = 'linear', cost = 10, scale = FALSE)
plot(svm.fit, dat)
summary(svm.fit)  #  there are 7 support vectors: 4 in class one and 3 in class two

# SVC classificaiton with linear kernel and cost = 0.1
svm.fit2 <- svm(y ~ ., data = dat, kernel = 'linear', cost = 0.1, scale = FALSE)
plot(svm.fit2, dat)
summary(svm.fit2)  # there are 16 support vectors: 8 in each class

# use tune function to determine optimal cost
tune.out <- tune(svm, y ~ ., data = dat, kernel = 'linear',
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)  # optimal cost parameter is 0.1

# simulated test set
xtest = matrix(rnorm(20 * 2), ncol = 2)
ytest = sample(c(-1, 1), 20, rep = TRUE)
xtest[ytest == 1,] = xtest[ytest == 1,] + 1

# predict class of test observations based on optimal model above
library(caret)
testdat = data.frame(x = xtest, y = as.factor(ytest))
yhat <- predict(tune.out$best.model, testdat)
confusionMatrix(yhat, testdat$y)


# SUPPORT VECTOR MACHINE
set.seed(1)
x <- matrix(rnorm(200*2), ncol = 2)
x[1:100,] = x[1:100,] + 2
x[101:150,] = x[101:150,] - 2

y <- c(rep(1,150),rep(2,50))

dat <- data.frame(x = x, y = as.factor(y))

plot(x, col = y)

train <- sample(200, 100)
svm.fit <- svm(y ~ ., data = dat[train,], kernel = 'radial', gamma = 1, cost = 1)
plot(svm.fit, dat[train,])
summary(svm.fit)

yhat <- predict(svm.fit, dat[-train,])
confusionMatrix(yhat, dat[-train, 'y'])

# increase cost value to reduce the training errors (but risk overfitting)
svm.fit <- svm(y ~ ., dat[train,], kernel = 'radial', gamma = 1, cost = 1e5)
plot(svm.fit, dat[train,])

# cross validation to determine parameters
set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat[train,],
                 kernel = 'radial',
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                               gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)

yhat <- predict(tune.out$best.model, dat[-train,])
confusionMatrix(yhat, dat[-train, 'y'])

# ROC Curves
library(ROCR)

rocplot <- function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, 'tpr', 'fpr')
  plot(perf, ...)
}

svm.opt <- svm(y ~ ., data = dat[train,], kernel = 'radial',
               gamma = 2, cost = 1, decision.values = TRUE)
fitted <- attributes(predict(svm.opt, dat[train,], decision.values = TRUE))$decision.values

rocplot(fitted, dat[train,'y'], main = 'Training Data')

# USING THE CARET PACKAGE
library(plyr)
dat$y <- revalue(dat$y, c("1" = "class1", "2" = "class2"))
ctrl <- trainControl(method = 'cv',
                     number = 10,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

svm.c <- train(y ~ ., dat[train,],
               method = 'svmRadial',
               trControl = ctrl,
               metric = "ROC")
svm.c
plot(svm.c$finalModel)

yhat.c <- predict(svm.c, dat[-train,])
confusionMatrix(yhat.c, dat[-train, 'y'])


# SVM WITH MULTIPLE CLASSES: one-versus-one approach

