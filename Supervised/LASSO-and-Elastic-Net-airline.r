# Craig Olson
# 2015-09-20
# flights.R

# Packages below should be installed
library(weatherData)
library(plyr)
library(dplyr)
library(data.table)
library(corrplot)
library(glmnet)

# memory.limit(size = 8000)

# Import data:  use file path of 1063608646_T_ONTIME_2013_11.csv
flight <- read.csv("~\\1063608646_T_ONTIME_2013_11.csv")

# create date column
flight$Date <- as.Date(paste(flight$YEAR, "-", flight$MONTH, "-", flight$DAY_OF_MONTH, sep = ""), format = "%Y-%m-%d")

# Removing unnessarily variables / variables unavailable for prediction
flight$ARR_DELAY <- NULL
flight$ORIGIN_CITY_NAME <- NULL
flight$DEST_CITY_NAME <- NULL # cities names essentially match ORIGIN and DEST
flight$YEAR <- NULL
flight$MONTH <- NULL # year and month are the same for all observations
flight$CANCELLED <- NULL
flight$CANCELLATION_CODE <- NULL
flight$CARRIER_DELAY <- NULL
flight$WEATHER_DELAY <- NULL
flight$NAS_DELAY <- NULL
flight$SECURITY_DELAY <- NULL
flight$LATE_AIRCRAFT_DELAY <- NULL

# get unique airport code
flight <- data.table(flight)
airports <- as.data.frame(unique(flight[,.(ORIGIN)]))
nrow(airports) # there are 305 unique airport codes
flight <- as.data.frame(flight)

# get November weather for each airport code
airports <- as.character(airports$ORIGIN)

missing <- as.vector(0L)
airport_weath <- matrix(nrow = 0, ncol = 24)
i <- 1

# Loop through all airports and pull Nov 2013 weather data; stored in airport_weath
for (a in airports){
  data <- getWeatherForDate(a, start_date = "2013-11-01", end_date = "2013-11-30", opt_all_columns = T)
  
  if(is.null(data)){
    missing[i] <- a
    i <- i + 1
    next
  }
  
  data[[2]] <- NULL
  data$ORIGIN <- a
  airport_weath <- rbind(airport_weath, data)
}

# T = Trace for precipitation, replace with 0
airport_weath$PrecipitationIn <- as.numeric(airport_weath$PrecipitationIn)
airport_weath$PrecipitationIn[is.na(airport_weath$PrecipitationIn)] <- 0

# Change Date format from POSIXlt to Date
airport_weath$Date <- as.Date(airport_weath$Date)

# FUnction to check how many NAs each column has (used throughout, doesn't need to be run)
check_col <- function(x){sum(is.na(x))}
#column_na_check <- apply(airport_weath, 2, check_col)

# Change NA and Missing in Events to "None"
airport_weath$Events[is.na(airport_weath$Events)] <- "None"
airport_weath$Events[airport_weath$Events == ""] <- "None"

# Keep airport weather data with complete cases.  The largest # of NA values is the 
# Max_Gust_SpeedMPH variable with 992.  This is over 10% of the airport data.
# Since Max Gust and Max Wind are highly correlated, I will remove the 
# Max Gust Speed variable from the analysis.
airport_weath$Max_Gust_SpeedMPH <- NULL

# Remove any remaining NAs (very few at this point)
airport_weath <- airport_weath[complete.cases(airport_weath),]

# make ORIGIN in weather data a factor to match flight data
airport_weath$ORIGIN <- as.factor(airport_weath$ORIGIN)
airport_weath$Date <- as.Date(airport_weath$Date)

# correlation plot of weather data
corr_data <- cor(airport_weath[c(2:20,22)])
corrplot(corr_data, method = "square", order = "AOE", tl.cex = .80, 
         tl.col = "black", mar = c(1,.5,1,.5), main = "Weather Correlation Matrix")

# I want to merge on both the ORIGIN and the DEST:
# the departure delay can be affected by weather in both the departure city and destination city
dep_airport_weath <- airport_weath
colnames(dep_airport_weath) <- paste("dep_", colnames(dep_airport_weath), sep = "")
dep_airport_weath <- rename(dep_airport_weath, c("dep_ORIGIN" = "ORIGIN", "dep_Date" = "Date"))

# Create data.frame with destination weather information
arr_airport_weath <- airport_weath
colnames(arr_airport_weath) <- paste("arr_", colnames(arr_airport_weath), sep = "")
arr_airport_weath <- rename(arr_airport_weath, c("arr_ORIGIN" = "DEST", "arr_Date" = "Date"))

# Merge flight delay data with airport weather data
flight_weath <- merge(x = flight, y = dep_airport_weath, by = c("ORIGIN", "Date"), all.x = TRUE)
flight_weath <- merge(x = flight_weath, y = arr_airport_weath, by = c("DEST", "Date"), all.x = TRUE)
  
# Remove rows where DEP_DELAY is NA
flight_weath <- flight_weath[!is.na(flight_weath$DEP_DELAY),]

# Remove airports with missing weather data (missing vector)
have_weather <- apply(flight_weath, 1, function(x) !any(x %in% missing))
flight_weath <- flight_weath[have_weather,]
# rm(list = "have_weather") # I need to remove large objects to conserve memory

# The weatheData package only has partial data for some airports.  Remove rows without weather data for departure or arrivals
flight_weath <- flight_weath[!is.na(flight_weath$dep_Mean_TemperatureF),]
flight_weath <- flight_weath[!is.na(flight_weath$arr_Mean_TemperatureF),]

# Create a week variable
flight_weath$week <- week(flight_weath$Date)

# Remove any additional unneccessary variables
flight_weath$ORIGIN <- NULL
flight_weath$Date <- NULL
flight_weath$FL_NUM <- NULL
flight_weath$DEST <- NULL
flight_weath$ARR_TIME <- NULL

flight_weath$dep_Events <- as.factor(flight_weath$dep_Events)
flight_weath$arr_Events <- as.factor(flight_weath$arr_Events)

# Look at the departure delay variable
par(mar = c(6,6,4,2))
hist(flight_weath$DEP_DELAY, main = "Histogram of Departure Delay", 
     xlab = "Departure Delay", ylab = "Frequency", col = "LightBlue")
summary(flight_weath$DEP_DELAY)

# Create design matrix, converting categorical variables into dummy variables, we now have 188 features
y <- flight_weath$DEP_DELAY
x <- model.matrix(DEP_DELAY ~ ., flight_weath)[,-1]

# rm(list = c("airport_weath", "arr_airport_weath", "dep_airport_weath", "flight",
#             "flight_weath", "airports"))

# Create 25% test set to validate model performance
set.seed(5696)
smp_size <- floor(0.75 * nrow(x))
train <- sample(seq_len(nrow(x)), size = smp_size)
test <- -train


# Null model for comparison (with only an intercept):  MSE is 908.8942
mean((mean(y[train]) - y[test]) ^ 2)


#--------------------------------------------------------------------------------------------------
# LASSO
# Run LASSO with cross validation to determine the optimal value of lambda (alpha = 1 for lasso)
flight_lasso_cv <- cv.glmnet(x[train,], y[train], alpha = 1, standardize = TRUE)

# Run LASSO without CV for solution path plot
flight_lasso <- glmnet(x[train,], y[train], alpha=1)

# Create Lasso Solution Path and Cross-Validation Plots
par(mfrow = c(1,2))
par(mar = c(6,4,6,2))
plot(flight_lasso, xvar="lambda", main = "LASSO Solution Path")
plot(flight_lasso_cv, main = "LASSO Cross-Validation Plot")

# choose best alpha for prediction
best_lambda <- flight_lasso_cv$lambda.1se  # 1.389558

# look at coefficients
coef(flight_lasso_cv, s = best_lambda)

# Make predictions on test set
delay_prediction <- predict(flight_lasso_cv, s = best_lambda, newx = x[test,])
mean((delay_prediction - y[test]) ^ 2) # Test MSE is 868.9009

# remove lasso objects
# rm(list = c("flight_lasso_cv", "flight_lasso"))

# ----------------------------------------------------------------------------------------------
# ELASTIC NET
# Run Elastic Net with cross validation to determine optimal lambda
flight_elastic_cv <- cv.glmnet(x[train,], y[train], alpha = 0.5, standardize = TRUE)

# Run Elastic Net without CV for solution path plot
flight_elastic <- glmnet(x[train,], y[train], alpha=0.5)

# create Elastic Net Solution Path and Cross Validation Plot
par(mfrow = c(1,2))
par(mar = c(6,4,6,2))
plot(flight_elastic, xvar="lambda", main = "Elastic Net Solution Path")
plot(flight_elastic_cv, main = "Elastic Net Cross-Validation Plot")

# Look at coefficients
best_lambda <- flight_elastic_cv$lambda.1se # 2.1023
coef(flight_elastic_cv, s = best_lambda)

# Make predictions on test set
delay_prediction2 <- predict(flight_elastic_cv, s = best_lambda, newx = x[-train,])
mean((delay_prediction2 - y[test]) ^ 2) # Test MSE is 864.7176

# remove elastic net objects
# rm(list = c("flight_elastic_cv", "flight_elastic"))

# Compare predicted vales with actual values
par(mfrow = c(1,1))
plot(delay_prediction2, y[test], main = "Predicted vs. Actual", xlab = "Predicted Delay",
     ylab = "Actual Delay")

