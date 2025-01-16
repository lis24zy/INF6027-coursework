# 1. Install and load packages
install.packages("forecast")
install.packages("ggplot2")
install.packages("ggcorrplot")
install.packages("zoo")
install.packages("tseries")
install.packages("psych")

library(forecast)
library(ggplot2)
library(ggcorrplot)
library(zoo)
library(tseries)
library(psych)

# 2. Reading and visualizing data
# Read data
rm(list=ls())
data <- read.csv("source.csv")
data$Time <- as.yearqtr(data$Time, format = "%Y Q%q")
# Mapping changes in the unemployment rate
ggplot(data, aes(x = Time, y = Unemp)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Unemployment Rate Over Time",
       x = "Time", y = "Unemployment Rate") +
  theme_minimal()
# Mapping changes in employment rates
ggplot(data, aes(x = Time, y = Emp)) +
  geom_line(color = "green", size = 1) +
  labs(title = "Employment Rate Over Time",
       x = "Time", y = "Employment Rate") +
  theme_minimal()
# Mapping changes in GDP growth rate
ggplot(data, aes(x = Time, y = GDP)) +
  geom_line(color = "red", size = 1) +
  labs(title = "GDP Growth Rate Over Time",
       x = "Time", y = "GDP Growth Rate") +
  theme_minimal()
# Mapping changes in GDP growth rate
ggplot(data, aes(x = Time, y = Inflation)) +
  geom_line(color = "purple", size = 1) +
  labs(title = "Inflation Rate Over Time",
       x = "Time", y = "Inflation Rate") +
  theme_minimal()
# Descriptive statistics 
descriptive_stats <- describe(data[, c("Unemp", "Emp", "GDP", "Inflation")])
print(descriptive_stats)


# 3. Analyze correlation
# Calculate correlation matrix
cor_matrix <- cor(data[, c("Unemp", "GDP", "Inflation", "Emp")])
print(cor_matrix)
# Visual correlation matrix
ggcorrplot(cor_matrix, method = "circle", type = "lower",
           title = "Correlation amang variables",
           lab = TRUE, lab_size = 4, colors = c("blue", "white", "red"))
# Add lag variable to analyze correlation
data$Lag1_GDP <- lag(data$GDP, k = 1)
data$Lag1_Inflation <- lag(data$Inflation, k = 1)
data$Lag1_Emp <- lag(data$Emp, k = 1)
# Recalculate the correlation matrix containing lag variables
cor_matrix_lag <- cor(data[, c("Unemp", "GDP", "Lag1_GDP", 
                               "Inflation", "Lag1_Inflation", 
                               "Emp", "Lag1_Emp")], 
                      use = "complete.obs")
print(cor_matrix_lag)
# Visualize the correlation matrix with lag variables
ggcorrplot(cor_matrix_lag, method = "circle", type = "lower",
           title = "Correlation added lagged value",
           lab = TRUE, lab_size = 4, colors = c("blue", "white", "red"))



# 4. Stability inspection and treatment
# (1) ADF test on unemployment rate p-value = 0.01934
adf_unemp <- adf.test(data$Unemp, alternative = "stationary")
print(adf_unemp)
data$Diff_Unemp <- c(NA, diff(data$Unemp, differences = 1))
adf_Diff_Unemp <- adf.test(na.omit(data$Diff_Unemp), alternative = "stationary")
print(adf_Diff_Unemp)
# (2) ADF test on employment rate p-value = 0.01
adf_emp <- adf.test(data$Emp, alternative = "stationary")
print(adf_emp)
data$Diff_Emp <- c(NA, diff(data$Emp, differences = 1))
adf_Diff_Emp <- adf.test(na.omit(data$Diff_Emp), alternative = "stationary")
print(adf_Diff_Emp)
data$Diff2_Emp <- c(NA, diff(data$Diff_Emp, differences = 1))
adf_Diff2_Emp <- adf.test(na.omit(data$Diff2_Emp), alternative = "stationary")
print(adf_Diff2_Emp)
# (3) ADF test on employment rate p-value = 0.01
adf_lag1_emp <- adf.test(data$Lag1_Emp, alternative = "stationary")
print(adf_lag1_emp)
data$Diff_Lag1_Emp <- c(NA, diff(data$Lag1_Emp, differences = 1))
adf_Diff_Lag1_Emp <- adf.test(na.omit(data$Diff_Lag1_Emp), alternative = "stationary")
print(adf_Diff_Lag1_Emp)
data$Diff2_Lag1_Emp <- c(NA, diff(data$Diff_Lag1_Emp, differences = 1))
adf_Diff2_Lag1_Emp <- adf.test(na.omit(data$Diff2_Lag1_Emp), alternative = "stationary")
print(adf_Diff2_Lag1_Emp)
# ADF test on GDP
adf_gdp <- adf.test(data$GDP, alternative = "stationary")
print(adf_gdp)
# ADF test on GDP p-value = 0.01566
adf_inflation <- adf.test(data$Inflation, alternative = "stationary")
print(adf_inflation)

# 5. Divide training set and test set
train_data <- subset(data, Time < as.yearqtr("2023 Q1"))
test_data <- subset(data, Time >= as.yearqtr("2023 Q1"))
# Ensure that the training set and test set have no missing values
train_data <- na.omit(train_data)
test_data <- na.omit(test_data)

# 6. Model training

# MODLE 1
train_xreg_emp <- as.matrix(train_data[, "Diff2_Emp", drop = FALSE])
test_xreg_emp <- as.matrix(test_data[, "Diff2_Emp", drop = FALSE])

arimax_model_emp <- auto.arima(train_data$Diff_Unemp, xreg = train_xreg_emp)
summary(arimax_model_emp)
# Prediction
forecast_emp <- forecast(arimax_model_emp, xreg = test_xreg_emp, h = nrow(test_data))

# Restore forecast data
initial_value_emp <- tail(train_data$Unemp, n = 1)
predicted_unemp_emp <- cumsum(c(initial_value_emp, forecast_emp$mean))[-1]
actual_unemp_emp <- test_data$Unemp
# Plotting forecast results
plot(test_data$Time, predicted_unemp_emp, type = "l", col = "blue", lwd = 2, 
     xlab = "Time", ylab = "Unemp", main = "Model 1",
     ylim = c(min(actual_unemp_emp, predicted_unemp_emp) - 0.2, 
              max(actual_unemp_emp, predicted_unemp_emp) + 0.2))
lines(test_data$Time, actual_unemp_emp, col = "red", lty = 2, lwd = 2)
legend("bottomright", legend = c("Predictive value", "Actual value"), 
       col = c("blue", "red"), lty = c(1, 2), cex = 0.6)

# Model 2
train_xreg_full <- as.matrix(train_data[, c("Diff2_Emp", "GDP", "Inflation")])
test_xreg_full <- as.matrix(test_data[, c("Diff2_Emp", "GDP", "Inflation")])

arimax_model_full <- auto.arima(train_data$Diff_Unemp, xreg = train_xreg_full)
summary(arimax_model_full)
# Prediction
forecast_full <- forecast(arimax_model_full, xreg = test_xreg_full, h = nrow(test_data))

# Restore forecast data
initial_value_full <- tail(train_data$Unemp, n = 1)
predicted_unemp_full <- cumsum(c(initial_value_full, forecast_full$mean))[-1]
actual_unemp_full <- test_data$Unemp
# Plotting forecast results
plot(test_data$Time, predicted_unemp_full, type = "l", col = "blue", lwd = 2, 
     xlab = "Time", ylab = "Unemp", main = "Model 2",
     ylim = c(min(actual_unemp_emp, predicted_unemp_emp) - 0.2, 
              max(actual_unemp_emp, predicted_unemp_emp) + 0.2))
lines(test_data$Time, actual_unemp_full, col = "red", lty = 2, lwd = 2)
legend("bottomright", legend = c("Predictive value", "Actual value"),
       col = c("blue", "red"), lty = c(1, 2), cex = 0.6)

# 7. model parameter
#  RMSE
rmse_emp <- sqrt(mean((test_data$Diff_Unemp - forecast_emp$mean)^2))
rmse_full <- sqrt(mean((test_data$Diff_Unemp - forecast_full$mean)^2))
cat("RMSE (MODLE 1):", rmse_emp, "\n")
cat("RMSE (MODEL 2):", rmse_full, "\n")

# 8. SAVE
# SAVE MODEL1
results_emp <- data.frame(
  Time = test_data$Time,
  Actual = test_data$Diff_Unemp,
  Predicted_Emp = forecast_emp$mean
)
write.csv(results_emp, "forecast_results_emp.csv", row.names = FALSE)

# SAVE MODEL2
results_full <- data.frame(
  Time = test_data$Time,
  Actual = test_data$Diff_Unemp,
  Predicted_Full = forecast_full$mean
)
write.csv(results_full, "forecast_results_full.csv", row.names = FALSE)



