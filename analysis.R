# Capital Bikeshare EDA & linear models (recovered from coursework session)
# Dataset: day.csv / hour.csv (Fanaee-T & Gama, UCI Bike Sharing Dataset)
# Requires: dplyr, ggplot2, lattice

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lattice)
# Set working directory if needed
# setwd("your-directory")
# Load the dataset
bike_data <- read.csv("day.csv")
# Check structure of the dataset
str(bike_data)
# Display first few rows
head(bike_data)
# (a) Convert weekday numbers to labels
bike_data$weekday <- factor(bike_data$weekday,
levels = 0:6,
labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
# Filter data for the year 2012 (yr == 1)
bike_2012 <- bike_data %>% filter(yr == 1)
# Compute mean number of registered users for each weekday
mean_registered_per_weekday <- bike_2012 %>%
group_by(weekday) %>%
summarise(mean_registered = mean(registered, na.rm = TRUE))
# Print the results
print(mean_registered_per_weekday)
# (b) Convert month number to month names
bike_2012$month <- factor(bike_2012$mnth,
levels = 1:12,
labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
# Plot density of registered users per month
ggplot(bike_2012, aes(x = registered, fill = month)) +
geom_density(alpha = 0.5) +
facet_wrap(~month, scales = "free") +
labs(title = "Density of Registered Users Per Month in 2012",
x = "Registered Users",
y = "Density") +
theme_minimal()
# (b) Load necessary library
library(ggplot2)
# Create a 2D level plot using contour fill
ggplot(bike_2012, aes(x = temp, y = hum, z = registered)) +
geom_contour_filled(aes(fill = ..level..)) +  # Filled contour levels
scale_fill_viridis_c(option = "plasma") +  # Better color scale
labs(title = "2D Level Plot of Registered Users by Temperature and Humidity",
x = "Temperature",
y = "Humidity",
fill = "Registered Users") +
theme_minimal()
library(lattice)
# Improved levelplot with proper color scaling
levelplot(registered ~ temp * hum, data = bike_2012,
col.regions = colorRampPalette(c("blue", "green", "yellow", "red"))(100),
xlab = "Temperature",
ylab = "Humidity",
main = "2D Levelplot of Registered Users by Temperature and Humidity",
cuts = 50)  # More divisions for smooth visualization
# (d) Function to sum registered users using a for loop
sum_registered_users <- function(data) {
total <- 0  # Initialize sum variable
for (i in 1:nrow(data)) {
total <- total + data$registered[i]
}
return(total)
}
# Apply function to 2012 data
total_registered_2012 <- sum_registered_users(bike_2012)
print(paste("Total Registered Users in 2012:", total_registered_2012))
# Compute mean registered users per year and season
mean_registered_tapply <- tapply(bike_data$registered,
list(bike_data$yr, bike_data$season),
mean, na.rm = TRUE)
# Print results
print(mean_registered_tapply)
# question 2
# Load necessary library
library(dplyr)
# Filter data for the year 2012
bike_2012 <- bike_data %>% filter(yr == 1)
# Fit linear regression model
model1 <- lm(casual ~ temp + hum, data = bike_2012)
# Display model summary
summary(model1)
# Potential Predictors to add are seasonal effects, whether it's a holiday or not, wind speed affecting bike usage, and day of the week.
# Fit a new linear model with additional predictors
model2 <- lm(casual ~ temp + hum + season + holiday + windspeed + weekday, data = bike_2012)
# Display model summary
summary(model2)
# Load required library
library(caret)
library(caret)
# Set number of repetitions
num_iterations <- 1000
cv_folds <- 5
# Function to perform cross-validation
cross_validation <- function(model_formula, data) {
errors <- c()
for (i in 1:num_iterations) {
# Create 5 folds
trainIndex <- createDataPartition(data$casual, p = 0.8, list = FALSE)
# Split into training and testing
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]
# Fit the model
model <- lm(model_formula, data = train_data)
# Make predictions
predictions <- predict(model, newdata = test_data)
# Compute sum of squared errors
SSE <- sum((test_data$casual - predictions)^2)
errors <- c(errors, SSE / (nrow(test_data) * num_iterations))
}
return(mean(errors))  # Return the mean SSE
}
# Perform cross-validation for both models
error_model1 <- cross_validation(casual ~ temp + hum, bike_2012)
error_model2 <- cross_validation(casual ~ temp + hum + season + holiday + windspeed + weekday, bike_2012)
# Print error results
print(paste("Mean SSE for Model 1:", error_model1))
print(paste("Mean SSE for Model 2:", error_model2))
# Compare models
if (error_model1 < error_model2) {
print("Model 1 has better predictive power!")
} else {
print("Model 2 has better predictive power!")
}
# Define response vector Y (Sales)
Y <- c(34, 55, 65, 87, 108, 110, 121, 125, 132)
# Print Y
print(Y)
# Define predictor matrix X (with intercept)
X <- cbind(1, c(23, 36, 47, 56, 56, 58, 64, 70, 72))
# Print X
print(X)
# Compute X transpose * X
XtX <- t(X) %*% X
# Print XtX
print(XtX)
# Compute X transpose * Y
XtY <- t(X) %*% Y
# Compute theta (regression coefficients)
theta <- solve(XtX) %*% XtY
# Print theta
print(theta)
# Convert data to a dataframe
marketing_data <- data.frame(Sales = Y, Advertisement = X[, 2])
# Run linear regression using lm()
model <- lm(Sales ~ Advertisement, data = marketing_data)
# Print model summary
summary(model)
# Define new data points
new_X <- cbind(1, c(53, 63))
new_Y <- c(96, 120)
# Learning rate (λ)
lambda <- 0.0001
# Compute predictions for new data
predictions <- new_X %*% theta
# Compute squared prediction error
error <- sum((new_Y - predictions)^2)
# Compute gradient
gradient <- -2 * t(new_X) %*% (new_Y - predictions)
# Update theta using SGD
theta_new <- theta - lambda * gradient
# Print updated theta
print(theta_new)
# question 5
# Load necessary libraries
library(lars)
library(lars)
