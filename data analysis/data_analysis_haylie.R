### using R 4.4.3 and RStudio 2024.12.1-563

# Install packages
install.packages("forecast")
install.packages("tseries")
install.packages("vars")
install.packages("readxl")
install.packages("lme4")
install.packages("tidyverse")
install.packages("summarytools")
install.packages("psych")
install.packages("corrplot")
install.packages("ggpubr")
install.packages("sjPlot")


# Load packages
library(tidyverse) # for data cleaning and manipulation
library(forecast)
library(tseries) 
library(vars) # for VAR model
library(readxl) # for reading xlsx files
library(summarytools) # to obtain frequency tables when checking for numbers and number of categories for a variable (column)
library(lme4) # for multilevel regression model
library(psych) # for correlation analysis
library(ggpubr) # for visualizing correlation
library(sjPlot) # to print regression model in a table


# Display as many decimal places as possible
options(scipen = 999)


# Load csv files
debt <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/debt_cleaned.csv", na.strings = c(" ", NA))
gas_prices <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/unleaded_gas_prices_cleaned.csv", na.strings = c(" ", NA))
household_income <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/household_income_cleaned.csv", na.strings = c(" ", NA)) # Quarterly
housing_price_increase <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/housing_price_increase_cleaned.csv", na.strings = c(" ", NA))
population_change <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/population_change_cleaned.csv", na.strings = c(" ", NA)) # Yearly (July - June)
cpi_combined <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/Multi-regression model.csv", na.strings = c(" ", NA))
diesel <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/diesel_cleaned.csv", na.strings = c(" ", NA))
# NOTE: I have changed the way names of GEO so that it is separated into City and Province. Also added a column, "covid", to indicate COVID periods
household_heating_fuel <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/household_heating_fuel_cleaned.csv", na.strings = c(" ", NA))
# NOTE: I have changed the way names of GEO so that it is separated into City and Province. Also added a column, "covid", to indicate COVID periods




# Combine all data frames together with CPI

# Trim the data frames first

debt_trim <- debt %>% 
  # We are only looking at the Revenue and Expenses
  filter(Central.government.operations == "B. Revenues" | Central.government.operations == "C. Expenses") %>% 
  # Rename variables so they are easily distinguishable in the main data frame
  rename(Government.balance = VALUE) %>%
  # Update object type
  mutate(REF_DATE = as.Date(ymd(REF_DATE))) %>% 
  # Select necessary columns only
  dplyr::select(REF_DATE, Central.government.operations, Government.balance) %>% 
  # Make central government operation components as the column names instead
  pivot_wider(names_from = Central.government.operations, values_from = Government.balance)


# National averages only
gas_prices_trim <- gas_prices %>% 
  filter(Province == "Canada") %>% 
  # Rename variables so they are easily distinguishable in the main data frame
  rename(Unleaded.gas.prices = VALUE) %>% 
  # Select necessary columns only
  dplyr::select(Unleaded.gas.prices, covid)

# Check gas price calculation for national average
gas_prices %>% 
  filter(Province != "Canada") %>% 
  group_by(REF_DATE) %>% 
  summarise(mean_gas_prices = mean(VALUE, na.rm = T))


household_income_trim <- household_income %>% 
  # The only parameters we are using
  filter(Wealth == "Total assets" | Wealth == "Total liabilities") %>% 
  # Change amounts for total liabilities into negatives
  mutate(VALUE = ifelse(Wealth == "Total liabilities", -VALUE, VALUE)) %>% 
  # Take out unnecessary column
  dplyr::select(-c(X, covid)) %>% 
  # Rename variables so they are easily distinguishable in the main data frame
  rename(Income.class = Characteristics,
         Amount = VALUE) %>% 
  # Change object type
  mutate(REF_DATE = as.Date(ymd(REF_DATE))) %>% 
  # Select necessary columns only
  dplyr::select(REF_DATE, Income.class, Wealth, Amount) %>% 
  # Make Income class-Wealths as the column names instead
  pivot_wider(names_from = c(Income.class, Wealth), values_from = Amount)

  
population_change_trim <- population_change %>% 
  # National numbers only
  filter(Province == "Canada" & Type.of.area == "Total CMA and CA") %>% 
  # These are the population change parameters that we're keeping
  filter(Components.of.population.growth == "Births" | Components.of.population.growth == "Deaths" | Components.of.population.growth == "Emigrants" | Components.of.population.growth == "Immigrants") %>% 
  rename(Number.of.people = VALUE) %>% 
  # Update REF_DATE to match over formatting
  mutate(REF_DATE = case_when(
    REF_DATE == "2017/2018" ~ "2018-01-01",
    REF_DATE == "2018/2019" ~ "2019-01-01",
    REF_DATE == "2019/2020" ~ "2020-01-01",
    REF_DATE == "2020/2021" ~ "2021-01-01",
    REF_DATE == "2021/2022" ~ "2022-01-01",
    REF_DATE == "2022/2023" ~ "2023-01-01",
    REF_DATE == "2023/2024" ~ "2024-01-01"
  )) %>% 
  # Change object type
  mutate(REF_DATE = as.Date(ymd(REF_DATE))) %>% 
  # Select necessary columns
  dplyr::select(REF_DATE, Components.of.population.growth, Number.of.people) %>% 
  # Make population change components as the column names instead
  pivot_wider(names_from = Components.of.population.growth, values_from = Number.of.people)


# Update object type of REF_DATE in cpi_combined
cpi_combined <- cpi_combined %>% 
  rename(REF_DATE = X) %>% 
  mutate(REF_DATE = as.Date(ym(REF_DATE)))

# # Add average diesel prices into cpi_combined data frame
# Average.diesel.prices <- diesel %>% 
#   group_by(REF_DATE) %>% 
#   summarise(Average.diesel.prices = mean(VALUE, na.rm = T)) %>% 
#   # Change object type
#   mutate(REF_DATE = as.Date(ymd(REF_DATE))) %>% 
#   ungroup()
# 
# cpi_combined <- left_join(cpi_combined, Average.diesel.prices, by = "REF_DATE")
# 
# # Add average household heating fuel prices into cpi_combined data frame
# Average.HFCE <- household_heating_fuel %>% 
#   group_by(REF_DATE) %>% 
#   summarise(Average.HFCE = mean(VALUE, na.rm = T)) %>% 
#   # Change object type
#   mutate(REF_DATE = as.Date(ymd(REF_DATE))) %>% 
#   ungroup()
# 
# cpi_combined <- left_join(cpi_combined, Average.HFCE, by = "REF_DATE")


# Merge dataframe

# Merge in gas_prices_trim
cpi_combined1 <- cbind(cpi_combined, gas_prices_trim)

# Merge in household_income_trim
cpi_combined2 <- left_join(cpi_combined1, household_income_trim, by = "REF_DATE") # There will be NAs for household_income columns as data is reported quarterly

# Merge in population_change_trim
cpi_combined3 <- left_join(cpi_combined2, population_change_trim, by = "REF_DATE") # There will be NAs for population_change columns as data is reported yearly

# Rearrange column so that "covid" is right after "REF_DATE"
cpi_combined3 <- cpi_combined3 %>% relocate(covid, .before = REF_DATE)

# # Merge in debt_trim
cpi_combined4 <- left_join(cpi_combined3, debt_trim, by = "REF_DATE")


# Copy the data frame so that it is not lost in data manipulation
cpi_main <- cpi_combined4


# Export main data frame
write.csv(cpi_main, "C:/Users/User/Desktop/UofC/W25/DATA 501/Data analysis/cpi_main.csv")



############# Merge CPI into gas_prices & diesel data frames separately ############# 


# Regular unleaded gasoline
gas_prices_cpi <- cbind(gas_prices, cpi_main$CPI.median)

gas_prices_cpi <- gas_prices_cpi %>% 
  # Filter out national averages
  filter(Province != "Canada") %>% 
  # Take out unnecessary column
  dplyr::select(-X) %>% 
  # Rename CPI.median
  rename(CPI.median = `cpi_main$CPI.median`)

# Change object type to date
gas_prices_cpi$REF_DATE <- as.Date(ymd(gas_prices_cpi$REF_DATE))

  
  
# Diesel
  
diesel_cpi <- cbind(diesel, cpi_main$CPI.median)

diesel_cpi <- diesel_cpi %>% 
  # Take out unnecessary column
  dplyr::select(-X) %>% 
  # Rename CPI.median
  rename(CPI.median = `cpi_main$CPI.median`)



# HFCE

household_heating_fuel_cpi <- cbind(household_heating_fuel, cpi_main$CPI.median)

household_heating_fuel_cpi <- household_heating_fuel_cpi %>% 
  # Take out unnecessary column
  dplyr::select(-X) %>% 
  # Rename CPI.median
  rename(CPI.median = `cpi_main$CPI.median`)
  



############## Research Questions ############## 

# Call the cpi_main data frame
cpi_main <- read.csv("C:/Users/user/Desktop/UofC/W25/DATA 501/Data analysis/cpi_main.csv")

# Take out the first column
cpi_main <- cpi_main %>% dplyr::select(-X)


## Now let's answer our RQs ##

# # Retrieve list of column names
# colnames(cpi_main)
# 
# # Standardize our data
# 
# cpi_main_standardized <- lapply(cpi_main[3:28], scale)


# RQ1: Which factor(s) lead/s to the most significant change in inflation rate in the 3 periods relative to the COVID-19 pandemic (Pre-COVID, COVID, Post-COVID)?

# # First, identify correlation between factors
# corr.test(cpi_main[3:28])
# 
# # By groups
# 
# # Housing price
# corrplot(cor(cpi_main[3:28]), type="upper", order="hclust")


# This is a mess ahhhh
# Will have to rethink on how we would like to do with this RQ
# summary(lm(CPI.median ~ Increased.housing.price.ratio + 
#           Average.diesel.prices + Average.HFCE  + Unleaded.gas.prices  + 
#           `All households_Total assets` + `All households_Total liabilities` + `Lowest income quintile_Total assets` + `Lowest income quintile_Total liabilities` + 
#           `Second income quintile_Total assets` + `Second income quintile_Total liabilities` + `Third income quintile_Total assets` + `Third income quintile_Total liabilities` +
#           `Fourth income quintile_Total assets` + `Fourth income quintile_Total liabilities` + `Highest income quintile_Total assets` + `Highest income quintile_Total liabilities` +
#           Births + Deaths + Immigrants + Emigrants +
#           `A. Budgetary balance` + `B. Revenues` + `C. Expenses`, 
#           data = cpi_main))




# RQ2: Which factor is the most significantly impacted by fluctuation in inflation rate in the 3 periods relative to the COVID-19 pandemic? 
# I am proposing to delete this RQ -- please see Notes for Final Report on OneDrive




# RQ3: How does inflation rate fluctuate, and whether the fluctuation varies across the 3 periods relative to the COVID-19 pandemic?  
summary(lm(CPI.median ~ covid, data = cpi_main))
# 1. All 3 COVID periods significantly predict change in inflation rate (p < 0.05)
# 2. R-squared = 0.5606, meaning that 56% of variation in inflation rate can be explained by COVID periods
# 3. Post-COVID has higher CPI median comparing to COVID, meaning inflation rate was the highest post-COVID
# 4. Pre-COVID has lower CPI median relative to COVID, meaning inflation rate was the lowest pre-COVID

# Careful with interpretation for this one****
date_cpi_lm <- summary(lm(CPI.median ~ REF_DATE, data = cpi_main))
date_cpi_lm

# predict(date_cpi_lm, newdata = data.frame("2017-10-01"))

#### **This RQ can be paired with a visualization to answer**




# RQ(x): What is the relationship between government's revenues and expenses and inflation rate?
summary(lm(CPI.median ~ B..Revenues + C..Expenses, data = cpi_main))
# p < 0.05 for Revenues, meaning that Revenues is a significant predictor of inflation rate
# For every unit increase in Revenues, inflation rate increases by 0.0001
# R-squared = 0.4242, meaning that 42.4% of variation in inflation can be explained by this model


# What about individual main effects?

summary(lm(CPI.median ~ B..Revenues, data = cpi_main))
# p < 0.05 for Revenues, meaning that Revenues is a significant predictor of inflation rate
# For every unit increase in Revenues, inflation rate increases by 0.0001
# R-squared = 0.4289, meaning that 42.9% of variation in inflation can be explained by this model

summary(lm(CPI.median ~ C..Expenses, data = cpi_main)) # non-sig




# RQ4: What is the relationship between household income and inflation rate?

# Very messy ahhh
# Don't know why it is NA for highest income-assets
summary(lm(CPI.median ~ All.households_Total.assets + All.households_Total.liabilities + Lowest.income.quintile_Total.assets + Lowest.income.quintile_Total.liabilities + 
             Second.income.quintile_Total.assets + Second.income.quintile_Total.liabilities + Third.income.quintile_Total.assets + Third.income.quintile_Total.liabilities +
             Fourth.income.quintile_Total.assets + Fourth.income.quintile_Total.liabilities + Highest.income.quintile_Total.assets + Highest.income.quintile_Total.liabilities, data = cpi_main))



# RQ5: What is the relationship between housing price increase and inflation rate?

# The model
housing_price_model <- lm(CPI.median ~ Increased.housing.price.ratio, data = cpi_main)

# Tests of assumptions

# 1. Normality
qqnorm(housing_price_model$residuals)
# Plot doesn't seem to follow a straight line 

# Log transform data so that CPI is a better fit for the regression model + Shapiro-Wilk test for normality
shapiro.test(lm(log(CPI.median) ~ Increased.housing.price.ratio, data = cpi_main)$residuals)
# p > 0.05, fail to reject null hypothesis; assumption of normality is met
# H0: data follows normal distribution
# Ha: data does not follow normal distribution


# 2. Linearity
plot(housing_price_model$residuals, housing_price_model$fitted) + abline(h = 0, col = "blue")
# Plot follows a specific pattern, linearity assumption is not met

# To test to see if Housing price is non-linear
summary(lm(CPI.median ~ Increased.housing.price.ratio + I(Increased.housing.price.ratio^2) + I(Increased.housing.price.ratio^3), data = cpi_main))

library(mgcv)
summary(gam(CPI.median ~ Increased.housing.price.ratio, data = cpi_main))


# 3. Constant variance

# Using Breusch-Pagan test
bptest(lm(log(CPI.median) ~ Increased.housing.price.ratio, data = cpi_main)) # p < 0.05
bptest(lm(1 / CPI.median ~ Increased.housing.price.ratio, data = cpi_main)) # p > 0.05, assumption of constant variance met




summary(lm(CPI.median ~ Increased.housing.price.ratio, data = cpi_main))
# 1. Increase in housing price ratio significant predicts change in inflation rate (p < 0.05)
# 2. R-squared = 0.8217, meaning that 82.17% of variation in inflation rate can be explained by Increase in housing price ratio
# 3. For every unit increase in housing price ratio, inflation rate will increase by 0.09




# RQ6: What is the relationship between population size and inflation rate?
summary(lm(CPI.median ~ Births + Deaths + Emigrants + Immigrants, data = cpi_main))
# None of the population components is predictive of change in inflation rate

# What about individual population components?
summary(lm(CPI.median ~ Births, data = cpi_main))
# 1. Births is significant in predicting change in inflation rate (p < 0.05)
# 2. R-squared = 0.6644, meaning 66.4% of variation in inflation rate can be explained by Births (the model is also statistically significant)
# 3. For every unit increase in Births, there is a 0.0001 decrease in inflation rate

summary(lm(CPI.median ~ Deaths, data = cpi_main))
# 1. Deaths is significant in predicting change in inflation rate (p < 0.05)
# 2. R-squared = 0.7, meaning 70% of variation in inflation rate can be explained by Deaths (the model is also statistically significant)
# 3. For every unit increase in Deaths, there is a 0.00005 increase in inflation rate

summary(lm(CPI.median ~ Emigrants, data = cpi_main)) # non-sig

summary(lm(CPI.median ~ Immigrants, data = cpi_main))
# 1. Immigrants is significant in predicting change in inflation rate (p < 0.05)
# 2. R-squared = 0.7216, meaning 72.16% of variation in inflation rate can be explained by Immigrants (the model is also statistically significant)
# 3. For every unit increase in Immigrants, there is a 0.00001 increase in inflation rate



# RQ7: What is the relationship between housing price trends and population growth, and how do they influence each other? 
summary(lm(Increased.housing.price.ratio ~ Births + Deaths + Emigrants + Immigrants, data = cpi_main))
# None of the population components is predictive of increase in housing price ratio

# What about individual population components?
summary(lm(Increased.housing.price.ratio ~ Births, data = cpi_main))
# 1. Births is significant in predicting housing price ratio (p < 0.05)
# 2. R-squared = 0.6516, meaning 65.16% of variation in housing price ratio can be explained by Births (the model is also statistically significant)
# 3. For every unit increase in Births, there is a 0.001 decrease in housing price ratio

summary(lm(Increased.housing.price.ratio ~ Deaths, data = cpi_main))
# 1. Deaths is significant in predicting housing price ratio (p < 0.05)
# 2. R-squared = 0.8659, meaning 86.59% of variation in housing price ratio can be explained by Deaths (the model is also statistically significant)
# 3. For every unit increase in Births, there is a 0.0006 increase in housing price ratio

summary(lm(Increased.housing.price.ratio ~ Emigrants, data = cpi_main)) # non-sig

summary(lm(Increased.housing.price.ratio ~ Immigrants, data = cpi_main))
# 1. Immigrants is significant in predicting housing price ratio (p < 0.05)
# 2. R-squared = 0.7462, meaning 74.62% of variation in housing price ratio can be explained by Immigrants (the model is also statistically significant)
# 3. For every unit increase in Immigrants, there is a 0.0001 increase in housing price ratio



# RQ8.1: What is the relationship between gas prices (both regular unleaded gasoline and diesel fuel) and inflation rate? 

# Regular unleaded gasoline
summary(lm(CPI.median ~ Unleaded.gas.prices, data = cpi_main))
# 1. Gas price is significant in predicting change in inflation rate (p < 0.05)
# 2. R-squared = 0.6869, meaning that 68.7% of variation in inflation rate can be explained by gas prices
# 3. For every unit increase in gas price, inflation rate increases by 0.036

# Diesel
summary(lm(CPI.median ~ Average.diesel.prices, data = cpi_main))
# 1. Diesel price is significant in predicting change in inflation rate (p < 0.05)
# 2. R-squared = 0.8214, meaning that 82.1% of variation in inflation rate can be explained by diesel prices
# 3. For every unit increase in gas price, inflation rate increases by 0.03

# RQ8.2: What is the relationship between HFCE and inflation rate? 
summary(lm(CPI.median ~ Average.HFCE, data = cpi_main))
# 1. HFCE is significant in predicting change in inflation rate (p < 0.05)
# 2. R-squared = 0.8693, meaning that 86.9% of variation in inflation rate can be explained by HFCE
# 3. For every unit increase in HFCE, inflation rate increases by 0.03

# RQ 8.3: What if we consider all gas prices?
summary(lm(CPI.median ~ Unleaded.gas.prices + Average.diesel.prices + Average.HFCE, data = cpi_main))
# 1. All types of gas, except for regular unleaded gasoline, are significant predictors of change in inflation rate (p < 0.05)
# 2. R-squared = 0.896, meaning 89.6% of variation in inflation rate can be explained by this model
# 3. In terms of significant predictors, for every unity increase in diesel price, inflation rate decreases by 0.05
# 4. For every unit increase in HFCE, inflation rate increases by 0.08



# RQ9: Is gas price (unleaded gasoline, diesel fuel, and HFCE) in each province dependent/independent of inflation rate?
# Reference: https://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r

##### For this one, we will have to manipulate the gas_prices data frame instead (add CPI median in the data frame) #####


## Correlation analysis

# Regular unleaded gasoline

# Let's check for assumptions before jumping to the actual test

# Assumption 1: Does the data follow normal distribution?
# Null hypothesis: data follows normal distribution (p > 0.05)

# Gas price
shapiro.test(gas_prices_cpi$VALUE) # p < 0.05, reject null hypothesis

# CPI median
shapiro.test(gas_prices_cpi$CPI.median) # p < 0.05, reject null hypothesis

# Since CPI median does not follow normal distribution, assumption 1 is not met
# We cannot use Pearson correlation test
# As CPI is continuous variable, we will use Spearman rank correlation coefficient instead

# Assumption 2: Linear relationship
ggscatter(gas_prices_cpi, x = "VALUE", y = "CPI.median",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Gas prices (in Cents per Litre)", ylab = "CPI median", title = "Correlation between Gas prices and CPI median")
# From the graph, the relationship is linear, assumption 2 is met

# Spearman correlation test
cor.test(gas_prices_cpi$VALUE, gas_prices_cpi$CPI.median, method = "spearman", exact = FALSE)
# p-value < 0.05, meaning that there is significant correlation between gas price and inflation rate
# H0: correlation coefficient, rho, equals to 0
# Ha: correlation coefficient, rho, does not equal to 0


# Diesel

# Let's check for assumptions before jumping to the actual test

# Assumption 1: Does the data follow normal distribution?
# Null hypothesis: data follows normal distribution (p > 0.05)

# Diesel price
shapiro.test(diesel_cpi$VALUE) # p < 0.05, fail to reject null hypothesis

# Since we already know from above that CPI median does not follow normal distribution, assumption 1 is not met
# We cannot use Pearson correlation test
# As CPI is continuous variable, we will use Spearman rank correlation coefficient instead

# Assumption 2: Linear relationship
ggscatter(diesel_cpi, x = "VALUE", y = "CPI.median",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Diesel prices (in Cents per Litre)", ylab = "CPI median", title = "Correlation between Diesel prices and CPI median")
# From the graph, the relationship is linear, assumption 2 is met

# Spearman correlation test
cor.test(diesel_cpi$VALUE, diesel_cpi$CPI.median, method = "spearman", exact = FALSE)
# p-value < 0.05, meaning that there is significant correlation between diesel price and inflation rate
# H0: correlation coefficient, rho, equals to 0
# Ha: correlation coefficient, rho, does not equal to 0


# HFCE

# Let's check for assumptions before jumping to the actual test

# Assumption 1: Does the data follow normal distribution?
# Null hypothesis: data follows normal distribution (p > 0.05)

# Diesel price
shapiro.test(household_heating_fuel_cpi$VALUE) # p < 0.05, fail to reject null hypothesis

# Since we already know from above that CPI median does not follow normal distribution, assumption 1 is not met
# We cannot use Pearson correlation test
# As CPI is continuous variable, we will use Spearman rank correlation coefficient instead

# Assumption 2: Linear relationship
ggscatter(household_heating_fuel_cpi, x = "VALUE", y = "CPI.median",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Household heating fuel expense (in Cents per Litre)", ylab = "CPI median", title = "Correlation between Household heating fuel expenses and CPI median")
# From the graph, the relationship is linear, assumption 2 is met

# Spearman correlation test
cor.test(household_heating_fuel_cpi$VALUE, household_heating_fuel_cpi$CPI.median, method = "spearman", exact = FALSE)
# p-value < 0.05, meaning that there is significant correlation between HFCE and inflation rate
# H0: correlation coefficient, rho, equals to 0
# Ha: correlation coefficient, rho, does not equal to 0


######### SCRAP THESE ######### 

# ## Linear mixed effect models
# 
# # Since we checked assumption of normality above, and all of the variables do not follow normal distribution, we will have to scale our data so that they are closer to normal
# 
# hist(gas_prices_cpi$VALUE) # data is not as normally distributed
# 
# hist(gas_prices_cpi$CPI.median)
# # data is right skewed, log transformation needed
# 
# # scale gas prices so that data fits better in regression model
# gas_prices_cpi$gas_prices_scaled <- scale(gas_prices_cpi$VALUE)
# 
# # Check distribution
# hist(gas_prices_cpi$gas_prices_scaled)
# 
# 
# # Q: Is gas price a significant predictor of inflation rate when random effects of between-province differences are accounted for?
# gas_prices_mlm <- lmer(log(CPI.median) ~ gas_prices_scaled + (1|Province), data = gas_prices_cpi)
# tab_model(gas_prices_mlm)
# 
# # Gas prices is significant predictor of CPI 


# ## ANOVAs
# 
# # Unleaded gasoline
# summary(aov(VALUE ~ CPI.median*Province, gas_prices_cpi))
# # p < 0.05 for both CPI and Province, meaning that gas prices in each province are significantly different, and that they also depend on inflation rate
# # Interaction term also has p < 0.05, meaning that impact of inflation differs between province
# 
# # Diesel price
# summary(aov(VALUE ~ CPI.median*Province, diesel_cpi))
# # p < 0.05 for both CPI and Province, meaning that gas prices in each province are significantly different, and that they also depend on inflation rate
# # Interaction term also has p < 0.05, meaning that impact of inflation differs between province
# 
# # HFCE
# summary(aov(VALUE ~ CPI.median*Province, household_heating_fuel_cpi))
# # p < 0.05 for both CPI and Province, meaning that gas prices in each province are significantly different, and that they also depend on inflation rate
# # Interaction term also has p < 0.05, meaning that impact of inflation differs between province



######### ######### ######### ######### 



## Regression models

# Unleaded gasoline
summary(lm(VALUE ~ Province*CPI.median, gas_prices_cpi))
# when looking at provinces alone, some provinces have lower gas prices than our reference (Alberta), when only province is being considered
# however, when inflation is being taken into account (interaction term), the effect became positive, meaning that these provinces are now predicted to have higher gas prices than Alberta when inflation rate is accounted for
# all interaction terms between inflation rate and provinces have p-values of less than 0.001, except for Northwest Territories and Ontario
# this means that gas price is not dependent on inflation rate in these 2 provinces
# R-squared = 0.7179, meaning 72% of variation in gas prices can be explained by both inflation rate and province, along with their interaction effects

# Diesel price
summary(lm(VALUE ~ Province*CPI.median, diesel_cpi))
# when looking at provinces alone, some provinces have lower diesel prices than our reference (Alberta), when only province is being considered
# however, when inflation is being taken into account (interaction term), the effect became positive, meaning that these provinces are now predicted to have higher diesel prices than Alberta when inflation rate is accounted for
# all interaction terms between inflation rate and provinces have p-values of less than 0.001, except for Northwest Territories, which they have a p-value of 0.013
# this means that diesel prices are dependent on inflation rate in all provinces
# R-squared = 0.8115, meaning 81% of variation in diesel prices can be explained by both inflation rate and province, along with their interaction effects

# HFCE
summary(lm(VALUE ~ Province*CPI.median, household_heating_fuel_cpi))
# when looking at provinces alone, some provinces have lower HFCE than our reference (BC), when only province is being considered
# however, when inflation is being taken into account (interaction term), the effect became positive, meaning that these provinces are now predicted to have higher HFCE than BC when inflation rate is accounted for
# only interaction terms between inflation rate and provinces (Manitoba, New Brunswick, Northwest Territories, Ontario, Quebec) reached statistical significance
# this means that HFCE are dependent on inflation rate in these provinces
# NOTE: HFCE data is not available for Alberta in StatsCan's database
# R-squared = 0.8423, meaning 84% of variation in diesel prices can be explained by both inflation rate and province, along with their interaction effects




#####################



#### Before we do this step, we should have already identified significant predictors of inflation****

# RQ10: Based on analysis of historical data (between October 2017 and December 2024), how will the inflation rate fluctuate in the next 2.5 - 3 years?

# This is where we use VAR

# Condition checking: Stationarity -- whether means and variances are constant over time and do not show any trending behaviour

# To do so, we conduct the Dickey-Fuller test

# Create a new data frame with only numeric data
cpi_main_numeric <- cpi_main %>% 
  dplyr::select(-c(covid, Household.heating.fuel.expense, Disel.fuel.at.self.service))

# Number of columns in the data frame
# n.col <- ncol(cpi_main)
n.col <- ncol(cpi_main_numeric)

# Create empty vectors to store results from the Dickey-Fuller test
col.name <- character(n.col) # column name
DF.value <- numeric(n.col) # test statistic
lag.order <- numeric(n.col) # lag order
DF.pval <- numeric(n.col) # p-value

# Create a for loop to store the result of the Dickey-Fuller test for each factor

# The column range starts from 3 because the first 2 columns of the data frame are non-numeric
# for(i in 3:ncol(cpi_main)){
#   DF.test.df <- cpi_main[i] # data frame to store the results
#   col.name[i] <- colnames(cpi_main[i])
#   DF.value[i] <- adf.test(na.omit(cpi_main[[i]]))$statistic
#   lag.order[i] <- as.numeric(adf.test(na.omit(cpi_main[[i]]))$parameter)
#   DF.pval[i] <- adf.test(na.omit(cpi_main[[i]]))$p.value
# }

for(i in 1:ncol(cpi_main_numeric)){
  DF.test.df <- cpi_main_numeric[i] # data frame to store the results
  col.name[i] <- colnames(cpi_main_numeric[i])
  DF.value[i] <- adf.test(na.omit(cpi_main_numeric[[i]]))$statistic
  lag.order[i] <- as.numeric(adf.test(na.omit(cpi_main_numeric[[i]]))$parameter)
  DF.pval[i] <- adf.test(na.omit(cpi_main_numeric[[i]]))$p.value
}

# Put the data in a data frame
DF.results.df <- data.frame(col.name, DF.value, lag.order, DF.pval)

DF.results.df <- DF.results.df %>% 
  # # First, take out the first 2 rows because data is non-numeric
  # filter(col.name != "") %>% 
  # Determine if the p-value is less than 0.05 (meaning that variable is stationary)
  mutate(Hypothesis = case_when(
    DF.pval <= 0.05 ~ "Stationary",
    DF.pval > 0.05 ~ "Not stationary"
  ))
# Only government revenues is stationary
# Differencing is needed to make the data more suitable for the forecasting model

ts_cpi_main_numeric <- ts(cpi_main_numeric, start = c(2017, 10), frequency = 12)
apply(ts_data, 2, adf.test)


### We have several options here to obtain data with same frequency: 
### We can either obtain monthly values for yearly/quarterly data, or do the opposite (obtain yearly values from monthly/quaterly data)




# First, select significant factors in predicting inflation

VARselect(df.lev, lag.max = 12, type = "const", season = 12)

# estimation
var.model_lev <- VAR(df.lev, p = 2, 
                     type = "const", season = 4)

# forecast of lev data
var.pred <- predict(var.model_lev, n.ahead = nhor)
x11(); par(mai=rep(0.4, 4)); plot(var.pred)
x11(); par(mai=rep(0.4, 4)); fanchart(var.pred)



