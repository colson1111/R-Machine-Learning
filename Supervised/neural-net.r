#  Taken from:  http://datascienceplus.com/fitting-neural-network-in-r/

set.seed(500)

library(MASS)

data <- Boston

# Check that there are no NAs
apply(data, 2, function(x) sum(is.na(x)))

# LINEAR REGRESSION FOR COMPARISON
# Split into training and test set
index <- sample(1:nrow(data), round(0.75 * nrow(data)))
train <- data[index,]
test <- data[-index,]

# run linear regression
lm.fit <- glm(medv ~ ., data = train)

summary(lm.fit)

pr.lm <- predict(lm.fit, test)
MSE.lm <- sum((pr.lm - test$medv) ^ 2)/ nrow(test)


# NEURAL NETWORK
# Step 1.  Data Preprocessing: normalization
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

# Step 2. Build Model:  Usually one hidden layer is enough.  # of Neurons should be
#   between the input layer and output layer sizes, usually 2/3 of the input size

# In this example:  2 hidden layers with 13:5:3:1 configuration:
#   The input layer has 13 inputs (13 features in the data set)
#   The two hidden layers have 5 and 3 neurons
#   The output layer has one output

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))

nn <- neuralnet(f, data = train_, hidden = c(5,3), linear.output = T) #linear.output = T for regression

# Graphical representation of the model
#   Black lines are connections between layers and the weights
#   Blue lines are the bias terms added at each step (like an intercept in a linear model)
plot(nn)

# Step 3.  Predictions
#   Need to 'un-scale' the predictions
pr.nn <- compute(nn, test_[,1:13])
pr.nn_ <- pr.nn$net.result * (max(data$medv) - min(data$medv)) + min(data$medv)
test.r <- (test_$medv) * (max(data$medv) - min(data$medv)) + min(data$medv)

MSE.nn <- sum((test.r - pr.nn_) ^ 2) / nrow(test_)

# Compare the test MSEs
print(paste(MSE.lm, MSE.nn))


# Visualize the comparison
par(mfrow = c(1,2))
plot(test$medv, pr.nn_, col = 'red', main = 'Real vs Predicted NN', pch = 18, cex = 0.7)
abline(0, 1, lwd = 2)
legend('bottomright', legend = 'NN', pch = 18, col = 'red', bty = 'n')

plot(test$medv, pr.lm, col = 'blue', main = 'Real vs Predicted LM', pch = 18, cex = 0.7)
abline(0, 1, lwd = 2)
legend('bottomright', legend = 'LM', pch = 18, col = 'blue', bty = 'n', cex = 0.95)

# Comparison plot on one graph
par(mfrow = c(1,1))
plot(test$medv, pr.nn_, col = 'red', main = 'Real vs Predicted NN', pch = 18, cex = 0.7)
points(test$medv, pr.lm, col = 'blue', pch = 18, cex = 0.7)
abline(0,1,lwd = 2)
legend('bottomright', legend = c('NN', 'LM'), pch = 18, col = c('red', 'blue'))


#  Using cross-validatin with NN
library(boot)
set.seed(200)

# Linear Regression
lm.fit <- glm(medv ~ ., data = data)
cv.glm(data, lm.fit, K = 10)$delta[1]

#  23.8356:  CV MSE for GLM

# Neural Net
set.seed(450)
cv.error <- NULL
k <- 10

library(plyr)
pbar <- create_progress_bar('text')
pbar$init(k)

for (i in 1:k){
  index <- sample(1:nrow(data), round(0.9*nrow(data)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]

  nn <- neuralnet(f, data = train.cv, hidden = c(5, 2), linear.output = T)

  pr.nn <- compute(nn, test.cv[,1:13])
  pr.nn <- pr.nn$net.result * (max(data$medv) - min(data$medv)) + min(data$medv)

  test.cv.r <- test.cv$medv * (max(data$medv) - min(data$medv)) + min(data$medv)

  cv.error[i] <- sum((test.cv.r - pr.nn) ^ 2) / nrow(test.cv)

  pbar$step()
}

mean(cv.error)

# create a boxplot
boxplot(cv.error, xlab = 'MSE CV', col = 'cyan',
        border = 'blue', names = 'CV error (MSE)',
        main = 'CV error (MSE) for NN', horizontal = TRUE)

# 10.327: CV MSE for NN
