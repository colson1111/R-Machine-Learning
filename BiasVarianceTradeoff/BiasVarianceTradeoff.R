
library(data.table)
library(ggplot2)
library(RColorBrewer)

generate_data = function(f, sample_size = 100){
  x = runif(sample_size, min=-4, max=5)
  y = f(x) + rnorm(sample_size, mean=0, sd=30)
  data.table(x,y)
}

f = function(x) {x^4 - x^3 + 2*x^2 - x + 5}

dat = generate_data(f, sample_size = 200)

ggplot(dat, aes(x=x,y=y)) +
  geom_point() +
  ggtitle("Simulated Data")


# create training and test datasets
set.seed(802)
smp = sample(x = 1:nrow(dat), size = round(.7*nrow(dat)), replace = F)
train = dat[smp,]
test = dat[-smp,]


# fit 3 models to the training data.  One underfit, one overfit, and one with 4 degree polynomial
plot(y~x, data=train, col='black', cex=1, pch=1)
points(y~x, data=test, col='red', cex=1, pch =1)
grid = seq(from = min(train$x), to = max(train$x), by = 0.01)

#display.brewer.all(3) # display color brewer palettes with n values
colors = brewer.pal(n=5, name="Set1")
j=1
for (i in c(1,4,25)){
  grid_predict = data.frame(x=grid)
  mod = lm(y~poly(x, degree=i), data=train)
  lines(grid, predict(mod, newdata=grid_predict), col=colors[j], lwd=3, lty=1)
  assign(paste0("model_",i), mod)
  j = j+1
}
title("Comparison of Polynomial Functions - Training and Test Data")
legend(x=-4, y =500, 
       legend = c("y ~ poly(x,1)", "y ~ poly(x,4)", "y ~ poly(x,25)"),
       col=colors, lty=1, lwd=2)

# Next, we want to run 10 models, calculate MSE on the test set, and plot: degrees vs. MSE
polys = c(2,3,4,5,8,10,15,20)
model_list = c()

for (i in 1:length(polys)){
  mod = lm(y ~ poly(x, degree=polys[i]), data=train)
  model_list[[i]] = mod
  assign(paste0("model_", polys[i]), mod)
}


# This is super ugly, but I'm getting weird data format issues from poly().
mod1 = lm(y ~ poly(x, degree=polys[1]), data=train)
mod2 = lm(y ~ poly(x, degree=polys[2]), data=train)
mod3 = lm(y ~ poly(x, degree=polys[3]), data=train)
mod4 = lm(y ~ poly(x, degree=polys[4]), data=train)
mod5 = lm(y ~ poly(x, degree=polys[5]), data=train)
mod6 = lm(y ~ poly(x, degree=polys[6]), data=train)
mod7 = lm(y ~ poly(x, degree=polys[7]), data=train)
mod8 = lm(y ~ poly(x, degree=polys[8]), data=train)


test_mse = c()
test_mse[1] = mean((test$y - predict(mod1, test)) ^ 2)
test_mse[2] = mean((test$y - predict(mod2, test)) ^ 2)
test_mse[3] = mean((test$y - predict(mod3, test)) ^ 2)
test_mse[4] = mean((test$y - predict(mod4, test)) ^ 2)
test_mse[5] = mean((test$y - predict(mod5, test)) ^ 2)
test_mse[6] = mean((test$y - predict(mod6, test)) ^ 2)
test_mse[7] = mean((test$y - predict(mod7, test)) ^ 2)
test_mse[8] = mean((test$y - predict(mod8, test)) ^ 2)

train_mse = c()
train_mse[1] = mean((train$y - predict(mod1, train)) ^ 2)
train_mse[2] = mean((train$y - predict(mod2, train)) ^ 2)
train_mse[3] = mean((train$y - predict(mod3, train)) ^ 2)
train_mse[4] = mean((train$y - predict(mod4, train)) ^ 2)
train_mse[5] = mean((train$y - predict(mod5, train)) ^ 2)
train_mse[6] = mean((train$y - predict(mod6, train)) ^ 2)
train_mse[7] = mean((train$y - predict(mod7, train)) ^ 2)
train_mse[8] = mean((train$y - predict(mod8, train)) ^ 2)


plot(x=polys, y=test_mse, type="l", col="red",
     ylim=c(min(min(train_mse), min(test_mse)/2),max(train_mse[1], test_mse[1])))
points(x=polys, y=test_mse, col="red", cex=1, pch=16)
points(x=polys, y=train_mse, type="l", col="blue")
points(x=polys, y=train_mse, col="blue", cex=1, pch=16)
legend("topleft",legend=c("Test Set", "Training Set"),
       col=c("red", "blue"), lty=1)
title("MSE vs. Model Complexity\nOn Training and Test Sets")








