
# random forest using a sample of the training set, success ~93% on hold out data

library(tree)
library(randomForest)

digits <- read.csv("C:\\Users\\Craig\\Desktop\\Kaggle\\Digits\\train.csv")

test <- read.csv("C:\\Users\\Craig\\Desktop\\Kaggle\\Digits\\test.csv")

sample <- sample(1:nrow(digits), size = 10000, replace = FALSE)

digit_samp <- digits[sample,]

# change response (label) to a factor
digit_samp$label <- as.factor(digit_samp$label)

#validate using test and training sets
set.seed(1)
train <- sample(1:nrow(digit_samp), round(nrow(digit_samp)*.7,2))

digit_train <- digit_samp[train,]
digit_test <- digit_samp[-train,]

results.test <- digit_samp$label[-train]

# Fit random forest
set.seed(1)
rf.digit <- randomForest(label ~ ., ntree = 100, data = digit_train, importance = TRUE)
rf.digit

# Validation
rf.pred <- predict(rf.digit, newdata = digit_test)
plot(rf.pred, results.test)
table(rf.pred, results.test)

# with only 10 trees, we get 87.60% of observation correct, much better than CART:
(131 + 166 + 130 + 126 + 139 + 109 + 136 + 148 + 99 + 130) / 1500
# with 100 trees, we get 93.13% of the test set correct, even better than 10 trees
(151 + 163 + 145 + 144 + 123 + 134 + 137 + 143 + 123 + 134) / 1500
# with 100 trees, and using a greater portion of the full data (10K instead of 5k):  got 93.96% of the test set correct
(287 + 351 + 284 + 267 + 251 + 246 + 303 + 298 + 246 + 286) / 3000

