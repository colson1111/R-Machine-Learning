# Keep variables with >0 variance
library(tree)
library(randomForest)
library(caret)

digits <- read.csv("~\\train.csv")
test <- read.csv("~\\test.csv")


# change response (label) to a factor
digits$label <- as.factor(digits$label)

# function get only zero variance
zerovar <- function(dat) {
  out <- lapply(dat, function(x) length(unique(x)))
  want <- which(!out > 1)
  unlist(want)
}

#  Instead of removing all white spaces, what if we split into black and white?
digitmat <- as.matrix(digits[,2:ncol(digits)])
digitmat <- ifelse(digitmat <= 255/2, 0, 1)
digits2 <- as.data.frame(digitmat)
digits2$label <- digits$label

zvar <- zerovar(digits2)
digits3 <- digits2[,-zvar]
digits3$label <- as.factor(digits3$label)



#  now we can ignore pixels with 0 variance and build a more complex model
#  and we only consider the difference between more black and more white
samp <- sample(1:nrow(digits3), size = 10000, replace = FALSE)
digit_samp <- digits3[samp,]

#validate using test and training sets
set.seed(1)
train <- sample(1:nrow(digit_samp), round(nrow(digit_samp)*.7,2))
digit_train <- digit_samp[train,]
digit_test <- digit_samp[-train,]
results.test <- digit_samp$label[-train]

# what if we use the whole tree only to classify between 4 or 9 or neither, can we get them correct?
# then we do a separate randomforest for each combination of digits, whichever # gets the most votes wins
i <- 0
j <- 0
test <- 1
rf.pred <- matrix(ncol = 90, nrow = nrow(digit_test))

for (i in 0:9){
  for (j in 0:9){
    if (i == j){next}

    digit_train_loop <- digit_train

    digit_train_loop$label <- ifelse(digit_train_loop$label == i, i, ifelse(digit_train_loop$label == j, j, -1))
    digit_train_loop$label <- factor(digit_train_loop$label)
    set.seed(1)
    rf.loop <- randomForest(label ~., ntree = 1, data = digit_train_loop, importance = TRUE, do.trace = TRUE)

    rf.pred[,test] <- predict(rf.loop, newdata = digit_test)
    
    test <- test + 1
  }
}

