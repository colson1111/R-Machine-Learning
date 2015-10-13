# Partial Least Squares and Principal Components Regression 
# Examples from: http://ccs1.hnue.edu.vn/hungtd/papers/PLS_Based_Methods/R-package-paper.pdf
library(pls)

data("yarn")
data("oliveoil")
data("gasoline")

options(digits = 4)

gasTrain <- gasoline[1:50, ]
gasTest <- gasoline[51:60, ]

# Partial Least Squares Regression
gas1 <- plsr(octane ~ NIR, ncomp = 10, data = gasTrain, validation = "LOO")

# model has 10 components and uses LOOCv to validation perf.

summary(gas1)
# Use Root Mean Squared Error of Prediction (RMSEP) to evaluate results

# plot the estimated RMSEPs against the # of components
# we see an 'elbow' at two, where RMSEP is 0.2966
plot(RMSEP(gas1), legendpos = "topright")

# Typically to get the same RMSEP performance, PCR will need more components

# Plot predicted versus actual with 2 components
plot(gas1, ncomp = 2, asp = 1, line = T)

# Score plot:  Look at the first three components for evidence of grouping or outliers
plot(gas1, plottype = "scores", comps = 1:3)

# Variance explained by each component
explvar(gas1)

# Make predictions on test set
predict(gas1, ncomp = 2, newdata = gasTest)

# Calculate the test RMSEP
RMSEP(gas1, newdata = gasTest) # 0.2445 for 2 components


# Loading Plot:  Look at peaks in the components plotted
plot(gas1, plottype = "loadings", comps = 1:2, legendpos = "topleft",
     labels = "numbers", xlab = "nm")  # labels=numbers makes the plot interpret the variables as numbers




#  TRY AGAIN USING PRINCIPAL COMPONENTS REGRESSION
dens1 <- pcr(density ~ NIR, ncomp = 5, data = yarn, validation = "LOO")

summary(dens1)

# plot RMSE against # of components
plot(RMSEP(dens1))

# plot predicted versus actual with 2 components
plot(dens1, ncomp = 2, asp = 1, line = T)

# Score plot:
plot(dens1, plottype = "scores", comps = 1:4)

# variance explained by each component
explvar(dens1) # the first two components explain 98.6% of variation

# Loading Plot
plot(dens1, plottype = "loadings", comps = 1:2,
     legendpos = "topleft", xlab = "nm")
