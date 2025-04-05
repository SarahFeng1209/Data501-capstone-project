############HFCE trend graph & Test statistic test######################
install.packages("readxl")   # If not installed
install.packages("ggplot2")  # For visualization
install.packages("forecast") # For trend analysis
install.packages("tseries")  # For statistical tests
install.packages("dplyr")    # For data manipulation

library(readxl)
library(ggplot2)
library(forecast)
library(tseries)
library(dplyr)


# Load the data
HFCE1 <- read_excel("C:/Users/fengm/Downloads/HFCE.xlsx")

# View the first few rows
head(HFCE1)

HFCE<- na.omit(HFCE1)
#TEST trend

adf_test <- adf.test(HFCE$VALUE, alternative = "stationary")
print(adf_test)

#RESULT:Dickey-Fuller = -20.756, Lag order = 22, p-value = 0.01 <0.05 RHo, Trend exist

colnames(HFCE)
# Plot the line graph
ggplot(HFCE, aes(x = REF_DATE, y = VALUE)) +
  geom_line(color = "blue", size = 1) +  # Blue line for clarity
  labs(title = "Trend of HFCE Value Over Time",
       x = "Date",
       y = "VALUE") +
  theme_minimal()

#Multiregression test regarding CPI, housing price, heating fuel expense and diesel fuel expense


rm(list=ls(all=TRUE))

# Load necessary libraries
install.packages("vars")
library(vars)
install.packages("readxl")
install.packages("tseries")  # For statistical tests
library(readxl)
library(tseries)


######################################################################################################################################
########Test Pre-Covid###########

# Read the Excel file
precovid <- read_excel("C:/Users/fengm/Downloads/Multi-regression model.xlsx", sheet = "precovid")
head(precovid)

var_data <- precovid[, c("CPI-median", "Increased housing price ratio", 
                     "Household heating fuel expense", "Disel fuel at self-service")]
# Check for missing values and remove if necessary
var_data <- na.omit(var_data)
# Convert to time series
ts_data <- ts(var_data, start=c(2017, 10), frequency=12)

#ADF TEST
#H0: The time series is non-stationary
#HA: The time series stationary

apply(ts_data, 2, adf.test)
#CPI-median
#Dickey-Fuller = -2.7011, Lag order = 3, p-value = 0.3039
#Increased housing price ratio
#Dickey-Fuller = -1.316, Lag order = 3, p-value = 0.8348
#Household heating fuel expense
#Dickey-Fuller = -2.4718, Lag order = 3, p-value = 0.3917
#Disel fuel at self-service
#Dickey-Fuller = -2.3584, Lag order = 3, p-value = 0.4352

#As we can see, all items are > 5%, so all four FRH0, they all non-stationary

#difference to make the series stationary
ts_data_diff <- diff(ts_data, differences=1)

#Selecting the optimal lag length
lag_selection <- VARselect(ts_data_diff, lag.max = 10, type = "const")
# Use AIC to avoid overfitting
optimal_lag <- lag_selection$selection["AIC(n)"]
#Displaying the results of lag selection criteria (AIC, BIC, HQC)
print(lag_selection)

#Fitting the VAR model with a lag of 5
var_model <- VAR(ts_data_diff, p = optimal_lag, type = "const")
#Displaying the summary of the VAR model
summary(var_model)

#all p-value> 5%, showing 
#Housing price ratio, heating fuel expense, and diesel fuel prices do not significantly influence CPI.median in this model.

colnames(var_model$y)
#Granger Causality Test
#The Granger causality test examines whether past values of one variable help predict another variable.
#H0:The variable X does not Granger-cause variable Y.
#(Past values of X do not provide additional predictive power for Y beyond Y’s own past values.)
#HA:The variable X does Granger-cause variable Y.

#The instantaneous causality test checks whether two variables are contemporaneously correlated (i.e., they change together at the same time).
#H0: There is no instantaneous causality between X and Y.
#(Changes in X at time t are not correlated with changes in Y at time t.)
#HA:There is instantaneous causality between X and Y.

#FRH0 at Granger causality test & RH0 at instantaneous causality test
causality(var_model, cause = "Increased.housing.price.ratio")
#FRH0 at Granger causality test & RH0 at instantaneous causality test
causality(var_model, cause = "Household.heating.fuel.expense")
#RH0 at Granger causality test & RH0 at instantaneous causality test
causality(var_model, cause = "Disel.fuel.at.self.service")





##############################################################################################################################
#######Test During COVID########

Covid <- read_excel("C:/Users/fengm/Downloads/Multi-regression model.xlsx", sheet = "Covid")
head(Covid)

var_data <- Covid[, c("CPI-median", "Increased housing price ratio", 
                     "Household heating fuel expense", "Disel fuel at self-service")]
# Check for missing values and remove if necessary
var_data <- na.omit(var_data)
# Convert to time series
ts_data <- ts(var_data, start=c(2020, 03), frequency=12)


#ADF TEST
#H0: The time series is non-stationary
#HA: The time series stationary

apply(ts_data, 2, adf.test)
#CPI-median
#Dickey-Fuller = -1.1581, Lag order = 3, p-value = 0.8954
#Increased housing price ratio
#Dickey-Fuller = -2.2269, Lag order = 3, p-value = 0.4856
#Household heating fuel expense
#Dickey-Fuller = -2.3272, Lag order = 3, p-value = 0.4472
#Diesel fuel at self-service
#Dickey-Fuller = -2.6159, Lag order = 3, p-value = 0.3365

#As we can see, all items are > 5%, so all four FRH0, they all non-stationary

#difference to make the series stationary
ts_data_diff <- diff(ts_data_cleaned, differences=1)

# Check stationarity after differencing
adf_results_diff <- apply(ts_data_diff, 2, adf.test)
print(adf_results_diff)

#Selecting the optimal lag length
lag_selection <- VARselect(ts_data_diff, lag.max = 11, type = "const")

# Use AIC to avoid over fitting
optimal_lag <- lag_selection$selection["AIC(n)"]

#Displaying the results of lag selection criteria (AIC, BIC, HQC)
print(lag_selection)

#Fitting the VAR model with a lag of optimal lag
var_model <- VAR(ts_data_diff, p = optimal_lag, type = "const")
#Displaying the summary of the VAR model
summary(var_model)

#Significant Variables (p < 0.05)
#Household.heating.fuel.expense.11 (p = 0.00479) → Positive effect on CPI-median.
#Disel.fuel.at.self.service.11 (p = 0.00492) → Negative effect on CPI-median.
#Household.heating.fuel.expense.12 (p = 0.01382) → Positive effect.
#Disel.fuel.at.self.service.12 (p = 0.02331) → Negative effect

colnames(var_model$y)
#Granger Causality Test
#The Granger causality test examines whether past values of one variable help predict another variable.
#H0:The variable X does not Granger-cause variable Y.
#(Past values of X do not provide additional predictive power for Y beyond Y’s own past values.)
#HA:The variable X does Granger-cause variable Y.

#The instantaneous causality test checks whether two variables are contemporaneously correlated (i.e., they change together at the same time).
#H0: There is no instantaneous causality between X and Y.
#(Changes in X at time t are not correlated with changes in Y at time t.)
#HA:There is instantaneous causality between X and Y.

#RH0 at Granger causality test & RH0 at instantaneous causality test
causality(var_model, cause = "Increased.housing.price.ratio")
#RH0 at Granger causality test & RH0 at instantaneous causality test
causality(var_model, cause = "Household.heating.fuel.expense")
#RH0 at Granger causality test & RH0 at instantaneous causality test
causality(var_model, cause = "Disel.fuel.at.self.service")





##############################################################################################################################
#######Test AFTER COVID########

postcovid <- read_excel("C:/Users/fengm/Downloads/Multi-regression model.xlsx", sheet = "postcovid")
head(postcovid)

var_data <- postcovid[, c("CPI-median", "Increased housing price ratio", 
                      "Household heating fuel expense", "Disel fuel at self-service")]
# Check for missing values and remove if necessary
var_data <- na.omit(var_data)
# Convert to time series
ts_data <- ts(var_data, start=c(2022, 08), frequency=12)


#ADF TEST
#H0: The time series is non-stationary
#HA: The time series stationary

apply(ts_data, 2, adf.test)
#CPI-median
#Dickey-Fuller = -1.543, Lag order = 3, p-value = 0.7478
#Increased housing price ratio
#Dickey-Fuller = -1.9112, Lag order = 3, p-value = 0.6067
#Household heating fuel expense
#Dickey-Fuller = -4.5494, Lag order = 3, p-value = 0.01
#Diesel fuel at self-service
#Dickey-Fuller = -4.6837, Lag order = 3, p-value = 0.01

#As we can see, 2 items are > 5%, so those 2 FRH0 and non-stationary

# Differencing only non-stationary variables
diff_CPI_median <- diff(ts_data[, "CPI-median"])
diff_Housing_price_ratio <- diff(ts_data[, "Increased housing price ratio"])

# Apply second differencing if necessary
if (adf.test(diff_CPI_median)$p.value > 0.05) {
  diff_CPI_median <- diff(diff_CPI_median)
}

if (adf.test(diff_Housing_price_ratio)$p.value > 0.05) {
  diff_Housing_price_ratio <- diff(diff_Housing_price_ratio)
}

# Adjust stationary variables
stationary_vars <- ts_data[-1, c("Household heating fuel expense", "Disel fuel at self-service")]

# Ensure all differenced variables have the same length
min_length <- min(length(diff_CPI_median), length(diff_Housing_price_ratio))

diff_CPI_median <- diff_CPI_median[1:min_length]
diff_Housing_price_ratio <- diff_Housing_price_ratio[1:min_length]
stationary_vars <- stationary_vars[1:min_length, ]

# Combine all differenced and stationary variables
postcovid_ts_diff <- cbind(diff_CPI_median, diff_Housing_price_ratio, stationary_vars)

# Convert to time series
postcovid_ts_diff <- ts(postcovid_ts_diff, start = c(2022, 10), frequency = 12)



# Re-check stationarity
apply(postcovid_ts_diff, 2, adf.test)

#Selecting the optimal lag length
lag_selection <- VARselect(ts_data_diff, lag.max = 11, type = "const")

# Use AIC to avoid over fitting
optimal_lag <- lag_selection$selection["AIC(n)"]

#Displaying the results of lag selection criteria (AIC, BIC, HQC)
print(lag_selection)

#Fitting the VAR model with a lag of optimal lag
var_model <- VAR(ts_data_diff, p = optimal_lag, type = "const")
#Displaying the summary of the VAR model
summary(var_model)

#Significant Variables (p < 0.05):

#Household.heating.fuel.expense.11 (p = 0.00479, positive effect) → A 1 unit increase in heating fuel expense at lag 11 is associated with an increase in CPI-median.

#Disel.fuel.at.self.service.11 (p = 0.00492, negative effect) → A 1 unit increase in diesel fuel expense at lag 11 leads to a decrease in CPI-median.

#Household.heating.fuel.expense.12 (p = 0.0138, positive effect) → This variable remains statistically significant at lag 12.

#Disel.fuel.at.self.service.12 (p = 0.02331, negative effect) → Diesel fuel price at lag 12 also has a strong impact.

colnames(var_model$y)
#Granger Causality Test
#The Granger causality test examines whether past values of one variable help predict another variable.
#H0:The variable X does not Granger-cause variable Y.
#(Past values of X do not provide additional predictive power for Y beyond Y’s own past values.)
#HA:The variable X does Granger-cause variable Y.

#The instantaneous causality test checks whether two variables are contemporaneously correlated (i.e., they change together at the same time).
#H0: There is no instantaneous causality between X and Y.
#(Changes in X at time t are not correlated with changes in Y at time t.)
#HA:There is instantaneous causality between X and Y.

#RH0 at Granger causality test & RH0 at instantaneous causality test
causality(var_model, cause = "Increased.housing.price.ratio")
#RH0 at Granger causality test & RH0 at instantaneous causality test
causality(var_model, cause = "Household.heating.fuel.expense")
#RH0 at Granger causality test & RH0 at instantaneous causality test
causality(var_model, cause = "Disel.fuel.at.self.service")







##################Predict CPI's trend in the next 2.5 - 3 years ###################################
library(readxl)
library(vars)
library(tseries)
library(forecast)

cpidata <- read_excel("C:/Users/fengm/Downloads/Multi-regression model.xlsx", sheet = "A")
colnames(cpidata)[1] <- "Date"
cpidata$Date <- as.Date(paste0(cpidata$Date, "-01"))  # convert to proper date if needed


# Extract relevant variables
ts_cpidata <- ts(cpidata[, -1], start = c(2017, 10), frequency = 12)

# Check for stationary (ADF Test)
apply(ts_cpidata, 2, adf.test)

# Not all stationary
ts_diff <- diff(ts_cpidata)


var_model <- VAR(ts_diff, lag.max = 12, ic = "AIC")
summary(var_model)

forecast_horizon <- 48
forecast_result <- predict(var_model, n.ahead = forecast_horizon)


# Extract CPI.median forecast (first column of $fcst list)
names(forecast_result$fcst)  # Check names if this fails
cpi_forecast_diff <- forecast_result$fcst[["CPI.median"]][, "fcst"]

#Invert differencing
last_cpi <- tail(ts_cpidata[, "CPI-median"], 1)
cpi_forecast <- cumsum(c(last_cpi, cpi_forecast_diff))[-1]


#Plot forecast
#Open a bigger plot window if needed

cpi_forecast <- ts(cpi_forecast, start = c(2024, 11), frequency = 12)

plot(ts_cpidata[, "CPI-median"], col = "blue", main = "CPI Forecast", ylab = "CPI", xlab = "Time", xlim = c(2017, 2028))
lines(cpi_forecast, col = "red", lty = 2)
legend("topleft", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = 1:2)

















############################VAR FOR GOVERNMENT EXPENSE AND CPI############################
# Load required libraries
# Load required packages
library(readxl)
library(vars)
library(tseries)
library(urca)

# Step 1: Load data
df <- read_excel("C:/Users/fengm/Downloads/Government Expense and CPI.xlsx", sheet = "A")
df$Date <- as.Date(paste0(df$Date, "-01"))

# Step 2: Create time series (remove Date column)
ts_data <- ts(df[, c("CPI-median", "Government Expense")], start = c(2017, 10), frequency = 12)

# Step 3: Check stationarity
apply(ts_data, 2, adf.test)

# Step 4: Difference data if needed
ts_diff <- diff(ts_data)

# Step 5: Fit VAR model (select lag with AIC)
var_model <- VAR(ts_diff, lag.max = 12, ic = "AIC")
summary(var_model)

causality(var_model, cause = "Government.Expense")

irf_result <- irf(var_model,
                  impulse = "Government.Expense",
                  response = "CPI.median",
                  boot = TRUE,
                  ci = 0.95,
                  n.ahead = 10)

# Extract values
irf_vals <- irf_result$irf$Government.Expense
upper_vals <- irf_result$Upper$Government.Expense
lower_vals <- irf_result$Lower$Government.Expense
periods <- 1:length(irf_vals)

# Custom Plot with axis label and legend
plot(periods, irf_vals, type = "l", col = "black", lwd = 2,
     ylim = range(c(lower_vals, upper_vals)),
     main = "Impulse Response: Effect of Government Expense on CPI",
     xlab = "Time Periods After Shock", ylab = "Effect on CPI-median")

# Confidence bands
lines(periods, upper_vals, col = "red", lty = 2)
lines(periods, lower_vals, col = "red", lty = 2)

# Add horizontal line at 0
abline(h = 0, col = "gray", lty = 3)

# Add legend
legend("topleft",
       legend = c("Impulse Response", "95% Confidence Interval (95% CI)"),
       col = c("black", "red"),
       lty = c(1, 2),
       lwd = c(2, 1),
       bty = "n")








###############VAR FOR HOUSEHOLD ALL ASSET AND CPI######################
# Load necessary libraries
library(readxl)
library(vars)

# Load the data set
income <- read_excel("C:/Users/fengm/Downloads/Household Income and CPI.xlsx", sheet = "A")

# Clean and prepare data
income <- income[, 1:3]
colnames(income) <- c("Date", "CPI", "Income")

# Remove rows with NA
data <- na.omit(income)

# Create time series object
ts_data <- ts(data[, c("CPI", "Income")], start = c(2017, 10), frequency = 4)

# Select lag length
lag_selection <- VARselect(ts_data, lag.max = 5, type = "const")
print(lag_selection$selection)

# Fit the VAR model using selected lag
var_model <- VAR(ts_data, p = lag_selection$selection["AIC(n)"], type = "const")

# Summary of the model
summary(var_model)

# Causality test (optional)
causality(var_model, cause = "Income")

# Impulse response function
irf_result <- irf(var_model,
                  impulse = "Income",
                  response = "CPI",
                  boot = TRUE,
                  ci = 0.95,
                  n.ahead = 10)

# Extract values for plotting
irf_vals <- irf_result$irf$Income
upper_vals <- irf_result$Upper$Income
lower_vals <- irf_result$Lower$Income
periods <- 1:length(irf_vals)

# Plot impulse response
plot(periods, irf_vals, type = "l", col = "black", lwd = 2,
     ylim = range(c(lower_vals, upper_vals)),
     main = "Impulse Response: Effect of Income on CPI",
     xlab = "Time Periods After Shock", ylab = "Effect on CPI")

# Add confidence bands
lines(periods, upper_vals, col = "red", lty = 2)
lines(periods, lower_vals, col = "red", lty = 2)

# Add horizontal line at 0
abline(h = 0, col = "gray", lty = 3)

# Add legend
legend("topleft",
       legend = c("Impulse Response", "95% Confidence Interval (95% CI)"),
       col = c("black", "red"),
       lty = c(1, 2),
       lwd = c(2, 1),
       bty = "n")

