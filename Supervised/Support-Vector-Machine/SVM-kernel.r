# Intro to Support Vector Machines
# http://cbio.ensmp.fr/~jvert/svn/tutorials/practical/svmbasic/svmbasic_notes.pdf 

# LINEAR SVM
n <- 150
p <- 2

sigma <- 1 # variance
meanpos <- 0 # center of distribution of positive examples
meanneg <- 1 # center of distribution of negative examples
npos <- round(n / 2) # number of positive examples
nneg <- n - npos # number of negative exaples

# generate data
xpos <- matrix(rnorm(npos * p, mean = meanpos, sd = sigma), npos, p)
xneg <- matrix(rnorm(nneg * p, mean = meanneg, sd = sigma), npos, p)
x <- rbind(xpos, xneg)

# generate labels
y <- matrix(c(rep(1, npos), rep(-1, nneg)))

# visualize the data
plot(x, col = ifelse(y > 0, 1, 2))
legend("topleft", c('Positive', 'Negative'), col = seq(2), pch = 1, text.col = seq(2))

# split into training and test sets
ntrain <- round(n * 0.8) # number of training samples
tindex <- sample(n, ntrain) # indices of training samples
xtrain <- x[tindex,]
xtest <- x[-tindex,]
ytrain <- y[tindex]
ytest <- y[-tindex]
istrain <- rep(0, n)
istrain[tindex] <- 1

# visualize
plot(x, col = ifelse(y > 0, 1, 2), pch = ifelse(istrain == 1, 1, 2))
legend("topleft", c('Positive Train', 'Positive Test', 'Negative Train', 'Negative Test'),
       col = c(1,1,2,2), pch = c(1,2,1,2), text.col = c(1,1,2,2))

# train the SVM
library(kernlab)

# train the svm
svp <- ksvm(xtrain, ytrain, type = "C-svc", kernel = 'vanilladot', C = 100, scaled = c())
svp # general summary
attributes(svp) # attributes you can access
alpha(svp) # the support vectors
alphaindex(svp)
b(svp)

# pretty plot of classifier
plot(svp, data = xtrain)

# predict labels on test
ypred <- predict(svp, xtest)
table(ytest, ypred)

# compute accuracy
sum(ypred == ytest) / length(ytest)

# compute the prediction scores
ypredscore <- predict(svp, xtest, type = "decision")

# check that the predicted labels are the signs of teh scores
table(ypredscore > 0, ypred)

# package to compute ROC curve
library(ROCR)

pred <- prediction(ypredscore, ytest)

# Plot ROC curve
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)

# Plot precision/recall curve
perf <- performance(pred, measure = "prec", x.measure = "rec")
plot(perf)

# plot accuracy as function of threshold
perf <- performance(pred, measure = "acc")
plot(perf)

# cross validation
svp <- ksvm(xtrain, ytrain, type = "C-svc", kernel = 'vanilladot', C = 1, scaled = c(), cross = 5)

# pretty plot of classifier
plot(svp, data = xtrain)

# predict labels on test
ypred <- predict(svp, xtest)
table(ytest, ypred)

# compute accuracy
sum(ypred == ytest) / length(ytest)

# compute the prediction scores
ypredscore <- predict(svp, xtest, type = "decision")

# check that the predicted labels are the signs of teh scores
table(ypredscore > 0, ypred)


# train a nonlinear SVM


sigma <- 1 # variance
ratepos <- 5 # center of distribution of positive examples
rateneg <- 2 # center of distribution of negative examples
npos <- round(n / 2) # number of positive examples
nneg <- n - npos # number of negative exaples

# generate data
xpos <- matrix(rexp(npos * p, rate = ratepos), npos, p)
xneg <- matrix(rnorm(nneg * p, mean = meanneg, sd = sigma), npos, p)
x <- rbind(xpos, xneg)

# generate labels
y <- matrix(c(rep(1, npos), rep(-1, nneg)))

# visualize the data
plot(x, col = ifelse(y > 0, 1, 2))
legend("topleft", c('Positive', 'Negative'), col = seq(2), pch = 1, text.col = seq(2))

# split into training and test sets
ntrain <- round(n * 0.8) # number of training samples
tindex <- sample(n, ntrain) # indices of training samples
xtrain <- x[tindex,]
xtest <- x[-tindex,]
ytrain <- y[tindex]
ytest <- y[-tindex]
istrain <- rep(0, n)
istrain[tindex] <- 1

# nonlinear SVM
svp <- ksvm(x, y, type = "C-svc", kernel = 'rbf', kpar = list(sigma = 1), C = 1)

plot(svp, data = x)
