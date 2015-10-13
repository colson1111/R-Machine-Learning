#  Example of the Adaboost.M1 Algorithm

# Boosting:  Repeatedly runs a weak learning algorithm and combines the classifiers
#   into a single classifier.
# http://cseweb.ucsd.edu/~yfreund/papers/IntroToBoosting.pdf

#install.packages("adabag")
#install.packages("mlbench")

library(adabag)
library(rpart)
library(mlbench)  # machine learning benchmark problems

# Vehicle Data
data(Vehicle)

# create training set
len <- length(Vehicle[,1])
train <- sample(1:len, 2 * len/3)

mfinal <- 100
maxdepth <- 5

# Build classification tree for comparison
Vehicle.rpart <- rpart(Class ~ ., data = Vehicle[train,], maxdepth = maxdepth)
Vehicle.rpart.pred <- predict(Vehicle.rpart, newdata = Vehicle[-train,], type = "class")
tb <- table(Vehicle.rpart.pred, Vehicle$Class[-train])
error.rpart <- 1 - (sum(diag(tb)) / sum(tb))
# error:  0.3156

# Build adaboost.M1 model
Vehicle.adaboost <- boosting(Class ~ ., data = Vehicle[train,], mfinal = mfinal, 
                                control = rpart.control(maxdepth = maxdepth))

Vehicle.adaboost.pred <- predict.boosting(Vehicle.adaboost, newdata = Vehicle[-train,])
#Vehicle.adaboost.pred$confusion
error.adaboost <- Vehicle.adaboost.pred$error

# adaboost, mfinal = 100, maxdepth = 5:  0.2411


# Build adaboost.M1 model with CV
Vehicle.adaboost.cv <- boosting.cv(Class ~ ., data = Vehicle, mfinal = mfinal, v = 10,
                                control = rpart.control(maxdepth = maxdepth))
# adaboost.cv, mfinal = 100, maxdepth = 5, v = 10:  0.2470
