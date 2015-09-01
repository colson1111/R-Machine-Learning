library(caret)
library(lmtest)
library(pscl)
library(MKmisc)
library(survey)
library(pROC)
library(ROCR)
library(MASS)
library(corrplot)
library(psych)
library(car)

data(GermanCredit)

Train <- createDataPartition(GermanCredit$Class, p = 0.6, list = FALSE)
training <- GermanCredit[Train,]
testing <- GermanCredit[-Train,]

# build logistic model on training data
mod_fit <- train(Class ~ Age + ForeignWorker + Property.RealEstate + Housing.Own +
                   CreditHistory.Critical, data = training, method = "glm", family = "binomial")

# Calculate odds ratio for each predictor
exp(coef(mod_fit$finalModel))

# predict testing data
predict(mod_fit, newdata = testing)


# build two models using glm
mod_fit_one <- glm(Class ~ Age + ForeignWorker + Property.RealEstate + Housing.Own +
                     CreditHistory.Critical, data = training, family = "binomial")
mod_fit_two <- glm(Class ~ Age + ForeignWorker, data = training, family = "binomial")

# ----------------------------------------------------------------------------------------
# GOODNESS OF FIT
# ----------------------------------------------------------------------------------------
# use likelihood ratio test to determine if the full model is better
# p value is < .05, provides evidence against the null hypothesis:  full model is better
  lrtest(mod_fit_one, mod_fit_two)

# McFadden's R^2:  Pseudo R^2 statistic
# the value is 0.078 - it is recommended we try to improve the model
  pR2(mod_fit_one)

# Hosmer-Lemeshow Test:  exaimnes whether the observed proportions of events are similar to the predicted
# probabilities of occurence in subgroups of the data set using a pearson chi square test
# Low p-value indicates a poor fit
  HLgof.test(fit = fitted(mod_fit_one), obs = training$Class)

# ----------------------------------------------------------------------------------------
# STATISTICAL TESTS FOR INDIVIDUAL PREDICTORS
# ----------------------------------------------------------------------------------------
# Wald Test: evaluate the statistical significance of each coefficient in the model
#  Foreign Worker alpha is greater than .05, suggests it can be omitted from the model
  regTermTest(mod_fit_one, "ForeignWorker")
  regTermTest(mod_fit_one, "CreditHistory.Critical")


# Variable Importance:  relative importance of individual predictors in the model
  varImp(mod_fit)

# ----------------------------------------------------------------------------------------
# VALIDATION
# ----------------------------------------------------------------------------------------

# Classification Rate
  pred = predict(mod_fit, newdata = testing)
  accuracy <- table(pred, testing[,"Class"])
  sum(diag(accuracy))/sum(accuracy)

  pred = predict(mod_fit, new data = testing)
  confusionMatrix(data = pred, testing$Class)

# ROC Curve
  # Compute AUC for predicting Class with the variable CreditHistory.Critical
  f1 = roc(Class ~ CreditHistory.Critical, data = training)
  plot(f1, col = "red")

  # Compute AUC for predicting Class with the model
  prob <- predict(mod_fit_one, newdata = testing, type = "response")
  pred <- prediction(prob, testing$Class)
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  plot(perf)

  auc <- performance(pred, measure = "auc")
  auc <- auc@y.values[[1]]
  auc


# ----------------------------------------------------------------------------------------
# OTHER DIAGNOSTICS
# ----------------------------------------------------------------------------------------

# Confidence Intervals
  confint(mod_fit_one) # selects the appropriate profile model
  confint.default(mod_fit_one) # ci's for the estimates using wald

  exp(confint(mod_fit_one)) # ci's for the odds ratios


# Correlation Coefficient
  cor(training$Age, training$ForeignWorker)  # two way correlation

  df <- training[,c("Age", "ForeignWorker", "Property.RealEstate", "Housing.Own", "CreditHistory.Critical")]
  cor(df) # correlation matrix

  M <- cor(df)
  corrplot(M)  # visualization of correlation matrix

  tbl = table(df$Housing.Own, df$CreditHistory.Critical)
  chisq.test(tbl)  # chi square test

  biserial(training$Age, training$CreditHistory.Critical)  # point biserial correlation
  # continuous x variable and a dichotomous y variable

# Variance Inflation Factor


# Influential Observations
  resid(mod_fit_one)
  mod_fit_one$residuals

  # partial residual pot against fitted values (explanatory variable)
  plot(residuals(mod_fit_one, type = "pearson") ~ training$Age, main = "pearson residual vs sex plot")

  # examine residuals and leverage
  rp = residuals(mod_fit_one, type = "pearson") # Pearson residuals
  rpstd = rstudent(mod_fit_one)  # Standardized pearson residuals
  lev = influence(mod_fit_one)$hat  # case leverages
  cookd = cooks.distance(mod_fit_one)  # cook's distances
  fv <- fitted(mod_fit_one)  # fitted values
  cbind(training$Class, fv, rp, rpstd, lev, cookd)


