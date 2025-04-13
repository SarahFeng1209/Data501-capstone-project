### using R 4.4.3 and RStudio 2024.12.1-563

# Install packages
install.packages("readxl")
install.packages("lme4")
install.packages("tidyverse")
# install.packages("summarytools")
install.packages("psych")
install.packages("ggpubr")
install.packages("sjPlot")


# Load packages
library(tidyverse) # for data cleaning and manipulation
library(readxl) # for reading xlsx files
# library(summarytools) # to obtain frequency tables when checking for numbers and number of categories for a variable (column)
library(lme4) # for multilevel regression model
library(psych) # for correlation analysis
library(ggpubr) # for visualizing correlation
library(sjPlot) # to print regression model in a table
library(car) # for Levene's test (test of homogeneity of variance)
library(MASS) # for Box-Cox transformation


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


  



############## Research Questions ############## 

# Call the cpi_main data frame
cpi_main <- read.csv("C:/Users/user/Desktop/UofC/W25/DATA 501/Data analysis/cpi_main.csv")

# Take out the first column
cpi_main <- cpi_main %>% dplyr::select(-X)




# RQ3: How does inflation rate fluctuate, and whether the fluctuation varies across the 3 periods relative to the COVID-19 pandemic?  

#### ANOVA ####

# Check assumptions of ANOVA

# Normality
by(cpi_main$CPI.median, cpi_main$covid, shapiro.test)
# p-values < 0.05 for all 3 COVID periods, assumption of normality is not met
# H0: Data is normally distributed
# Ha: Data is not normally distributed

# Homoscedasticity
leveneTest(CPI.median ~ covid, data = cpi_main)
# p-value < 0.05, assumption of equal variance is not met
# H0: equal variances in data
# Ha: unequal variances in data

# Since both assumptions are not met, a non-parametric test is used instead of ANOVA

# Kruskal-Wallis test
kruskal.test(CPI.median ~ covid, data = cpi_main)
# Test-statistic = 43.818, degrees of freedom = 2, p < 0.001
# This means that inflation rate differs significantly across the 3 COVID periods

cpi_main %>% 
  mutate(covid = factor(covid, levels = c("Pre-COVID", "COVID", "Post-COVID"))) %>% 
  ggplot(aes(x = covid, y = CPI.median, color = covid)) + geom_boxplot() +
  xlab("COVID periods") + ylab("CPI Median") + ggtitle("Boxplot of CPI Median across COVID periods") + 
  scale_color_manual(name = "COVID Periods", values = c("Pre-COVID" = "#619CFF", "COVID" = "#F8766D", "Post-COVID" = "#00BA38"))


#### **This RQ can be paired with a visualization to answer**




################## Pattern Discovery ################## 


# RQ(x): What is the relationship between government expenditure and inflation rate across the 3 COVID periods?

# Code for Expenses can be found in "code for VAR model & HFCE trend.R"

# Revenues

# First, visualization (also available in EDA)
cpi_main %>% 
  mutate(covid = factor(covid, levels = c("Pre-COVID", "COVID", "Post-COVID"))) %>% 
  ggplot(aes(x = REF_DATE, y = B..Revenues, color = covid, group = covid)) + geom_point() + geom_line() + 
  xlab("Month") + ylab("Revenues (in million dollars)") + 
  ggtitle("Trend of Government Revenues by Month") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_color_manual(name = "COVID Periods", values = c("Pre-COVID" = "#619CFF", "COVID" = "#F8766D", "Post-COVID" = "#00BA38"))
# From the visualization, we can see an overall increasing trend in government revenues over time


# Model
revenues_model <- lm(CPI.median ~ B..Revenues, data = cpi_main)

# Assumptions

# Check normality of residuals
qqnorm(revenues_model$residuals)
qqline(revenues_model$residuals)
# residuals follow a straight line

# Shapiro-Wilk test to cross reference results from visualization
shapiro.test(residuals(revenues_model)) # p < 0.05, normality assumption is not met

# Power transform
boxcox(revenues_model) # lambda = -1.5 is the best

revenues_model_boxcox <- lm(CPI.median^(-1.5) ~ B..Revenues, data = cpi_main)

# Check normality again
shapiro.test(residuals(revenues_model_boxcox)) # p > 0.05, normality assumption is met

# Check constant variance + linearity
plot(revenues_model_boxcox$residuals ~ revenues_model_boxcox$fitted.values) 
# there seems to be a subtle U-shaped pattern

# Breusch-Pagan test to cross reference results from visualization
bptest(revenues_model_boxcox) # p > 0.05, constant variance assumption is met

# Results of the regression model
summary(revenues_model_boxcox)
# 1. Government revenue significant predicts inflation rate (p < .001)
# 2. R-squared = 0.5831, meaning 58% of variation in inflation rate can be explained by government revenues in this model
# 3. For each unit increase in government revenue, inflation rate will decrease by 0.00001
# formula: 1 / CPI^1.5 = 0.73 - 0.00001 * Revenues

ggplot(data = cpi_main, aes(x = B..Revenues, y = CPI.median^(-1.5))) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "blue") + 
  xlab("Revenues") + ylab("CPI") +
  ggtitle("Predicted CPI by Revenues")





# RQ5: What is the relationship between housing price increase and inflation rate in the context of the 3 COVID periods?

### UPDATED RQ5: Does increase in housing price index predict change in inflation rate?


# First, visualization (also available in EDA)
ggplot(data = cpi_main, aes(x = REF_DATE, y = Increased.housing.price.ratio, color = covid, group = covid)) + geom_point() + 
  xlab("Month") + ylab("Increase in Housing Price Index Comparing to December 2016") + 
  ggtitle("Trend of Increase in Housing Price Index by Month") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# From the visualization, we can see that there are 3 distinct trends identified across the 3 periods relative to COVID



# Filter data from main data frames
precovid <- cpi_main %>% 
  filter(covid == "Pre-COVID")

duringcovid <- cpi_main %>% 
  filter(covid == "COVID")

postcovid <- cpi_main %>% 
  filter(covid == "Post-COVID")


######## Pre-COVID ######## 
precovid_housing <- lm(CPI.median ~ Increased.housing.price.ratio, data = precovid)

# Assumptions

# Check normality of residuals
qqnorm(precovid_housing$residuals)
qqline(precovid_housing$residuals)
# residuals follow a straight line

# Shapiro-Wilk test to cross reference results from visualization
shapiro.test(residuals(precovid_housing)) # marginally significant, still assumes test of normality passes

# Check constant variance + linearity
plot(precovid_housing$residuals ~ precovid_housing$fitted.values) 
# residuals are randomly distributed around r = 0
# variance is approximately constant for all fitted values

# Breusch-Pagan test to cross reference results from visualization
bptest(precovid_housing) # p > 0.05, constant variance assumption is met

# Results of the regression model
summary(precovid_housing)
# 1. Increase in housing price index significant predicts inflation rate in the pre-COVID period (p < .001)
# 2. R-squared = 0.5577, meaning 56% of variation in inflation rate pre-COVID can be explained by increase in housing price index in this model
# 3. For each unit increase in housing price index, inflation rate will decrease by 0.44
# formula: CPI = 3.19 - 0.44 * Increased.housing.price.index.ratio

ggplot(data = precovid, aes(x = Increased.housing.price.ratio, y = CPI.median)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "blue") + 
  xlab("Increase in Housing Price Index") + ylab("CPI") +
  ggtitle("Predicted CPI by Increase in Housing Price Index in pre-COVID period")



######## ######## 


######## During COVID ######## 
duringcovid_housing <- lm(CPI.median ~ Increased.housing.price.ratio, data = duringcovid)

# Assumptions

# Check normality of residuals
qqnorm(duringcovid_housing$residuals)
qqline(duringcovid_housing$residuals)
# residuals seem to follow a straight line

# Shapiro-Wilk test to cross reference results from visualization
shapiro.test(residuals(duringcovid_housing)) # p > 0.05, normality assumption is met

# Check constant variance + linearity
plot(duringcovid_housing$residuals ~ duringcovid_housing$fitted.values) 
# U-shape pattern, meaning that data does not fit in a linear model

# Breusch-Pagan test to cross reference results from visualization
bptest(duringcovid_housing) # p < 0.05, constant variance assumption is not met

# Add a squared term of x
duringcovid_housing_poly <- lm(CPI.median ~ Increased.housing.price.ratio + I(Increased.housing.price.ratio^2), data = duringcovid)

plot(duringcovid_housing_poly$residuals ~ duringcovid_housing_poly$fitted.values) 
# residuals now randomly scattered around r = 0, and variances seem to be constant for fitted values

# Results of the regression model
summary(duringcovid_housing_poly)
# 1. Increase in housing price index significant predicts inflation rate during COVID (p < .001)
# 2. The coefficient for x^2 means curvature. Since coefficient is close to 0, the curve is nearly linear in this model
# 3. R-squared = 0.9859, meaning 99% of variation in inflation rate during COVID can be explained by increase in housing price index in this model
# 4. For each unit increase in housing price index, inflation rate will decrease by 0.07
# formula: CPI = 2.09 - 0.07 * Increased.housing.price.index.ratio + 0.006 * Increased.housing.price.index.ratio^2

ggplot(duringcovid, aes(x = Increased.housing.price.ratio, y = CPI.median)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "blue") + 
  xlab("Increase in Housing Price Index") + ylab("CPI") +
  ggtitle("Curved Effect of Increase in Housing Price Index on CPI during COVID")

# Compare which model is better
anova(duringcovid_housing, duringcovid_housing_poly)
# Quadratic term is significant (p < .001), which means that model is a better fit for prediction of inflation during COVID

######## ######## 


######## Post-COVID ######## 
postcovid_housing <- lm(CPI.median ~ Increased.housing.price.ratio, data = postcovid)

# Assumptions

# Check normality of residuals
qqnorm(postcovid_housing$residuals)
qqline(postcovid_housing$residuals)
# Patterns follow a straight line

# Shapiro-Wilk test to cross reference results from visualization
shapiro.test(residuals(postcovid_housing)) # p > 0.05, normality assumption is met

# Check constant variance + linearity
plot(postcovid_housing$residuals ~ postcovid_housing$fitted.values) 
# residuals are randomly distributed around r = 0
# variance is approximately constant for all fitted values

# Breusch-Pagan test to cross reference results from visualization
bptest(postcovid_housing) # p > 0.05, constant variance assumption is met

# Results of the regression model
summary(postcovid_housing)
# 1. Increase in housing price index significant predicts inflation rate in the post-COVID period (p < .001)
# 2. R-squared = 0.6562, meaning 66% of variation in inflation rate in the post-COVID period can be explained by increase in housing price index in this model
# 3. For each unit increase in housing price index, inflation rate will increase by 0.89
# formula: CPI = -21.27 + 0.89 * Increased.housing.price.index.ratio

ggplot(postcovid, aes(x = Increased.housing.price.ratio, y = CPI.median)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") + 
  xlab("Increase in Housing Price Index") + ylab("CPI") +
  ggtitle("Predicted CPI by Increase in Housing Price Index in post-COVID period")

######## ######## 




# RQ6: What is the relationship between population size and inflation rate?

#### UPDATED RQ6: Does change in population size predict change in inflation rate?

# Visualization can be found from EDA

# Births: steady trend across the years
# Deaths: steady trend across the years
# Emigrants: steady trend across the years
# Immigrants: decrease from 2017 to 2021; increase from 2021 to 2024
# Even though it might be beneficial to do 2 models for immigrants, there are only 7 data points available in total, in which the sample size would not be big enough to make meaningful statistical inferences


# All population components
summary(lm(CPI.median ~ Births + Deaths + Emigrants + Immigrants, data = cpi_main))
# None of the population components is predictive of change in inflation rate


#####  What about individual population components? ##### 

##### Births ##### 

births_model <- lm(CPI.median ~ Births, data = cpi_main)

# Check normality
qqnorm(births_model$residuals)
# residuals follow a straight line

# Shapiro-Wilk test to cross reference results from visualization
shapiro.test(residuals(births_model)) # p > 0.05, normality assumption is met

# Check constant variance + linearity
plot(births_model$residuals ~ births_model$fitted.values) 
# residuals are randomly distributed around r = 0
# variance is approximately constant for all fitted values

# Breusch-Pagan test to cross reference results from visualization
bptest(births_model) # p > 0.05, constant variance assumption is met

# Results of regression model
summary(births_model)
# 1. Births is significant in predicting change in inflation rate (p = 0.0157)
# 2. R-squared = 0.6644, meaning 66.4% of variation in inflation rate can be explained by Births (the model is also statistically significant)
# 3. For every unit increase in Births, there is a 0.0001 decrease in inflation rate
# formula: CPI.median = 44.94 - 0.0001 * Births

ggplot(cpi_main, aes(x = Births, y = CPI.median)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") + 
  xlab("Births") + ylab("CPI") +
  ggtitle("Predicted CPI by Number of Births")


##### Deaths ##### 

deaths_model <- lm(CPI.median ~ Deaths, data = cpi_main)

# Check normality
qqnorm(deaths_model$residuals)
# residuals follow a straight line

# Shapiro-Wilk test to cross reference results from visualization
shapiro.test(residuals(deaths_model)) # p > 0.05, normality assumption is met

# Check constant variance + linearity
plot(deaths_model$residuals ~ deaths_model$fitted.values) 
# Funnel shape pattern, indicating unequal variances

# Breusch-Pagan test to cross reference results from visualization
bptest(deaths_model) # p < 0.05, constant variance assumption is not met

# Log transform the model
deaths_model_log <- lm(log(CPI.median) ~ Deaths, data = cpi_main)

# Check constant variance again
bptest(deaths_model_log) # p-value is still less than 0.05

# Box-Cox transformation to determine the best power transformation
boxcox(deaths_model, lambda = seq(-6, 2, by = 1)) # lambda = -2 is the best

# New boxcox model
deaths_model_boxcox <- lm(CPI.median^(-2) ~ Deaths, data = cpi_main)

# Check constant variance again
bptest(deaths_model_boxcox) # p > 0.05, constant variance assumption is met

# Results of regression model
summary(deaths_model_boxcox)
# 1. Deaths is significant in predicting change in inflation rate (p = 0.00187)
# 2. R-squared = 0.853, meaning that 85% of variation in inflation rate can be explained by Deaths
# 3. For every unit increase in Deaths (a person in this case), there would be a 0.000005 decrease in inflation rate
# formula: 1 / CPI.median^2 = 1.49 - 0.000005 * Deaths

ggplot(cpi_main, aes(x = Deaths, y = CPI.median^(-2))) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") + 
  xlab("Deaths") + ylab("CPI") +
  ggtitle("Predicted CPI by Number of Deaths")


##### Emigrants ##### 

emigrants_model <- lm(CPI.median ~ Emigrants, data = cpi_main)

# Check normality
qqnorm(emigrants_model$residuals)
# residuals follow a straight line

# Shapiro-Wilk test to cross reference results from visualization
shapiro.test(residuals(emigrants_model)) # p > 0.05, normality assumption is met

# Check constant variance + linearity
plot(emigrants_model$residuals ~ emigrants_model$fitted.values) 
# residuals are randomly distributed around r = 0
# variance is approximately constant for all fitted values

# Breusch-Pagan test to cross reference results from visualization
bptest(emigrants_model) # p > 0.05, constant variance assumption is met

# Results of regression model
summary(emigrants_model)
# Emigrants is not significant predictor of change in inflation

ggplot(cpi_main, aes(x = Emigrants, y = CPI.median)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") + 
  xlab("Emigrants") + ylab("CPI") +
  ggtitle("Predicted CPI by Number of Emigrants")


##### Immigrants ##### 

immigrants_model <- lm(CPI.median ~ Immigrants, data = cpi_main)

# Check normality
qqnorm(immigrants_model$residuals)
# residuals follow a straight line

# Shapiro-Wilk test to cross reference results from visualization
shapiro.test(residuals(immigrants_model)) # p > 0.05, normality assumption is met

# Check constant variance + linearity
plot(immigrants_model$residuals ~ immigrants_model$fitted.values) 
# residuals are randomly distributed around r = 0
# variance is approximately constant for all fitted values

# Breusch-Pagan test to cross reference results from visualization
bptest(immigrants_model) # p > 0.05, constant variance assumption is met

# Results of regression model
summary(immigrants_model)
# 1. Immigrants is significant in predicting change in inflation rate (p = 0.00965)
# 2. R-squared = 0.7216, meaning 72% of variation in inflation rate can be explained by Immigrants (the model is also statistically significant)
# 3. For every unit increase in Immigrants, there is a 0.000098 increase in inflation rate
# formula: CPI.median = -0.62 + 0.0000098 * Immigrants

ggplot(cpi_main, aes(x = Immigrants, y = CPI.median)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") + 
  xlab("Immigrants") + ylab("CPI") +
  ggtitle("Predicted CPI by Number of Immigrants")



# RQ7: What is the relationship between housing price trends and population growth, and how do they influence each other? 
summary(lm(Increased.housing.price.ratio ~ Births + Deaths + Emigrants + Immigrants, data = cpi_main))
# None of the population components is predictive of increase in housing price ratio

# What about individual population components?

##### Births #####

births_housing <- lm(Increased.housing.price.ratio ~ Births, data = cpi_main)

# Check normality
qqnorm(births_housing$residuals)
# residuals follow a straight line

# Shapiro-Wilk test to cross reference results from visualization
shapiro.test(residuals(births_housing)) # p > 0.05, normality assumption is met

# Check constant variance + linearity
plot(births_housing$residuals ~ births_housing$fitted.values) 
# residuals are randomly distributed around r = 0
# variance is approximately constant for all fitted values

# Breusch-Pagan test to cross reference results from visualization
bptest(births_housing) # p > 0.05, constant variance assumption is met

# Results of regression model
summary(births_housing)
# 1. Births is significant in predicting increase in housing price index (p = 0.0174)
# 2. R-squared = 0.6516, meaning 65.16% of variation in housing price ratio can be explained by Births (the model is also statistically significant)
# 3. For every unit increase in Births, there is a 0.001 decrease in housing price ratio
# formula: Increased.housing.price.ratio = 453.54 - 0.001 * Births



##### Deaths #####

deaths_housing <- lm(Increased.housing.price.ratio ~ Deaths, data = cpi_main)

# Check normality
qqnorm(deaths_housing$residuals)
# residuals follow a straight line

# Shapiro-Wilk test to cross reference results from visualization
shapiro.test(residuals(deaths_housing)) # p > 0.05, normality assumption is met

# Check constant variance + linearity
plot(deaths_housing$residuals ~ deaths_housing$fitted.values) 
# residuals are randomly distributed around r = 0
# variance is approximately constant for all fitted values

# Breusch-Pagan test to cross reference results from visualization
bptest(deaths_housing) # p > 0.05, constant variance assumption is met

# Results of regression model
summary(deaths_housing)
# 1. Deaths is significant in predicting increase in housing price index (p= 0.00148)
# 2. R-squared = 0.8659, meaning 86.59% of variation in housing price ratio can be explained by deaths (the model is also statistically significant)
# 3. For every unit increase in deaths, there is a 0.0006 increase in housing price ratio
# formula: Increased.housing.price.ratio = -135.24 + 0.0006 * Deaths


##### Emigrants #####

emigrants_housing <- lm(Increased.housing.price.ratio ~ Emigrants, data = cpi_main)

# Check normality
qqnorm(emigrants_housing$residuals)
# residuals follow a straight line

# Shapiro-Wilk test to cross reference results from visualization
shapiro.test(residuals(emigrants_housing)) # p > 0.05, normality assumption is met

# Check constant variance + linearity
plot(emigrants_housing$residuals ~ emigrants_housing$fitted.values) 
# residuals are randomly distributed around r = 0
# variance is approximately constant for all fitted values

# Breusch-Pagan test to cross reference results from visualization
bptest(emigrants_housing) # p > 0.05, constant variance assumption is met

# Results of regression model
summary(emigrants_housing) # non-sig


##### Immigrants #####

immigrants_housing <- lm(Increased.housing.price.ratio ~ Immigrants, data = cpi_main)

# Check normality
qqnorm(immigrants_housing$residuals)
# residuals follow a straight line

# Shapiro-Wilk test to cross reference results from visualization
shapiro.test(residuals(immigrants_housing)) # p > 0.05, normality assumption is met

# Check constant variance + linearity
plot(immigrants_housing$residuals ~ immigrants_housing$fitted.values) 
# residuals are randomly distributed around r = 0
# variance is approximately constant for all fitted values

# Breusch-Pagan test to cross reference results from visualization
bptest(immigrants_housing) # p > 0.05, constant variance assumption is met

# Results of regression model
summary(immigrants_housing)
# 1. Immigrants is significant in predicting increase in housing price index (p = 0.00759)
# 2. R-squared = 0.7462, meaning 75% of variation in housing price ratio can be explained by Immigrants (the model is also statistically significant)
# 3. For every unit increase in Immigrants, there is a 0.0001 increase in housing price ratio
# formula: Increased.housing.price.ratio = -22.84 + 0.0001 * Immigrants



# RQ8.1: What is the relationship between gas prices (both regular unleaded gasoline and diesel fuel) and inflation rate? 


# First, visualization (also available in EDA)
ggplot(data = cpi_main, aes(x = REF_DATE, y = Unleaded.gas.prices, color = covid, group = covid)) + geom_point() + geom_line() +
  xlab("Month") + ylab("Regular unleaded gasoline price (Cents per Litre") + 
  ggtitle("Trend of Regular Unleaded Gasoline Price by Month") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# From the visualization, we can see that there are 3 distinct trends identified across the 3 periods relative to COVID


#### Regular unleaded gasoline

######## Pre-COVID ######## 
precovid_gas <- lm(CPI.median ~ Unleaded.gas.prices, data = precovid)

# Assumptions

# Check normality of residuals
qqnorm(precovid_gas$residuals)
qqline(precovid_gas$residuals)
# residuals follow a straight line

# Shapiro-Wilk test to cross reference results from visualization
shapiro.test(residuals(precovid_gas)) # p > 0.05, normality assumption is met

# Check constant variance + linearity
plot(precovid_gas$residuals ~ precovid_gas$fitted.values) 
# residuals are randomly distributed around r = 0
# variance is approximately constant for all fitted values

# Breusch-Pagan test to cross reference results from visualization
bptest(precovid_gas) # p > 0.05, constant variance assumption is met

# Results of the regression model
summary(precovid_gas) # non-sig


######## ######## 


######## During COVID ######## 
duringcovid_gas <- lm(CPI.median ~ Unleaded.gas.prices, data = duringcovid)

# Assumptions

# Check normality of residuals
qqnorm(duringcovid_gas$residuals)
qqline(duringcovid_gas$residuals)
# residuals follow a straight line

# Shapiro-Wilk test to cross reference results from visualization
shapiro.test(residuals(duringcovid_gas)) # p > 0.05, normality assumption is met

# Check constant variance + linearity
plot(duringcovid_gas$residuals ~ duringcovid_gas$fitted.values) 
# residuals are randomly distributed around r = 0
# variance is approximately constant for all fitted values

# Breusch-Pagan test to cross reference results from visualization
bptest(duringcovid_gas) # p > 0.05, constant variance assumption is met

# Results of the regression model
summary(duringcovid_gas)
# 1. Gas price significant predicts inflation rate during COVID (p < .001)
# 2. R-squared = 0.9217, meaning 92% of variation in inflation rate during COVID can be explained by gas prices in this model
# 3. For each unit increase in gas price (cents per litre), inflation rate will increase by 0.03
# formula: CPI = -1.46 + 0.03 * Unleaded.gas.prices

ggplot(duringcovid, aes(x = Unleaded.gas.prices, y = CPI.median)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") + 
  xlab("Regular Unleaded Gasoline Price (in Cents per Litre)") + ylab("CPI") +
  ggtitle("Predicted CPI by Regular Unleaded Gasoline Price during COVID")


######## ######## 


######## Post-COVID ######## 
postcovid_gas <- lm(CPI.median ~ Unleaded.gas.prices, data = postcovid)

# Assumptions

# Check normality of residuals
qqnorm(postcovid_gas$residuals)
# Residuals do not seem to follow a straight line

# Shapiro-Wilk test to cross reference results from visualization
shapiro.test(residuals(postcovid_gas)) # p < 0.05, normality assumption is not met

# Log transform the data
postcovid_gas_log <- lm(log(CPI.median) ~ Unleaded.gas.prices, data = postcovid)

# Check normality again
shapiro.test(residuals(postcovid_gas_log)) # p > 0.05, normality assumption is now met

# Check constant variance + linearity
plot(postcovid_gas_log$residuals ~ postcovid_gas_log$fitted.values) 
# residuals are randomly distributed around r = 0
# variance is approximately constant for all fitted values

# Breusch-Pagan test to cross reference results from visualization
bptest(postcovid_gas_log) # p > 0.05, constant variance assumption is met

# Results of the regression model
summary(postcovid_gas_log) # non-sig

######## ######## 


# First, visualization (also available in EDA)
cpi_main %>% 
  mutate(covid = factor(covid, levels = c("Pre-COVID", "COVID", "Post-COVID"))) %>% 
  ggplot(aes(x = REF_DATE, y = Disel.fuel.at.self.service, color = covid, group = covid)) + geom_point() + geom_line() +
  xlab("Month") + ylab("Total Diesel Price (Cents per Litre)") + 
  ggtitle("Trend of Total Diesel Price by Month") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_color_manual(name = "COVID Periods", values = c("Pre-COVID" = "#619CFF", "COVID" = "#F8766D", "Post-COVID" = "#00BA38"))
# From the visualization, we can see that there are 3 distinct trends identified across the 3 periods relative to COVID

diesel %>% 
  mutate(covid = factor(covid, levels = c("Pre-COVID", "COVID", "Post-COVID"))) %>% 
  ggplot(aes(x = REF_DATE, y = VALUE, group = covid, colour = covid)) + geom_line() + geom_point() + 
  xlab("Month") + ylab("Gas Price (Cents per Litre)") + ggtitle("Line Graph of Canadian Gas Prices by Month and COVID Periods") + 
  scale_color_manual(name = "COVID Periods", values = c("Pre-COVID" = "#619CFF", "COVID" = "#F8766D", "Post-COVID" = "#00BA38"))


# Diesel

######## Pre-COVID ######## 
precovid_diesel <- lm(CPI.median ~ Disel.fuel.at.self.service, data = precovid)

# Assumptions

# Check normality of residuals
qqnorm(precovid_diesel$residuals)
qqline(precovid_diesel$residuals)
# residuals follow a straight line

# Shapiro-Wilk test to cross reference results from visualization
shapiro.test(residuals(precovid_diesel)) # p > 0.05, normality assumption is met

# Check constant variance + linearity
plot(precovid_diesel$residuals ~ precovid_diesel$fitted.values) 
# residuals are randomly distributed around r = 0
# variance is approximately constant for all fitted values

# Breusch-Pagan test to cross reference results from visualization
bptest(precovid_diesel) # p > 0.05, constant variance assumption is met

# Results of the regression model
summary(precovid_diesel)
# 1. Diesel prices are significant in predicting inflation rate in pre-COVID period (p = 0.009)
# 2. R-squared = 0.1979, meaning that 20% of variation in inflation rate can be explained by diesel prices in this model
# 3. For each unit increase in diesel price, inflation rate will increase by 0.0009
# formula: CPI = 0.03 + 0.0009 * Diesel.price

ggplot(precovid, aes(x = Disel.fuel.at.self.service, y = CPI.median)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") + 
  xlab("Total Diesel Price (in Cents per Litre)") + ylab("CPI") +
  ggtitle("Predicted CPI by Total Diesel Price in pre-COVID period")


######## ######## 


######## During COVID ######## 
duringcovid_diesel <- lm(CPI.median ~ Disel.fuel.at.self.service, data = duringcovid)

# Assumptions

# Check normality of residuals
qqnorm(duringcovid_diesel$residuals)
qqline(duringcovid_diesel$residuals)
# residuals follow a straight line

# Shapiro-Wilk test to cross reference results from visualization
shapiro.test(residuals(duringcovid_diesel)) # p > 0.05, normality assumption is met

# Check constant variance + linearity
plot(duringcovid_diesel$residuals ~ duringcovid_diesel$fitted.values) 
# residuals are randomly distributed around r = 0
# variance is approximately constant for all fitted values

# Breusch-Pagan test to cross reference results from visualization
bptest(duringcovid_diesel) # p > 0.05, constant variance assumption is met

# Results of the regression model
summary(duringcovid_diesel)
# 1. Diesel price significant predicts inflation rate during COVID (p < .001)
# 2. R-squared = 0.9681, meaning 97% of variation in inflation rate during COVID can be explained by diesel prices in this model
# 3. For each unit increase in diesel price (cents per litre), inflation rate will increase by 0.002
# formula: CPI = -1.03 + 0.002 * Diesel.price

ggplot(duringcovid, aes(x = Disel.fuel.at.self.service, y = CPI.median)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") + 
  xlab("Total Diesel Price (in Cents per Litre)") + ylab("CPI") +
  ggtitle("Predicted CPI by Total Diesel Price during COVID")


######## ######## 


######## Post-COVID ######## 
postcovid_diesel <- lm(CPI.median ~ Disel.fuel.at.self.service, data = postcovid)

# Assumptions

# Check normality of residuals
qqnorm(postcovid_diesel$residuals)
qqline(postcovid_diesel$residuals)
# Residuals do not seem to follow a straight line

# Shapiro-Wilk test to cross reference results from visualization
shapiro.test(residuals(postcovid_diesel)) # p < 0.05, normality assumption is not met


# Add a quadratic term + log transform y
postcovid_diesel_poly <- lm(log(CPI.median) ~ Disel.fuel.at.self.service + I(Disel.fuel.at.self.service^2), data = postcovid)

# Check normality again
shapiro.test(residuals(postcovid_diesel_poly)) # normality assumption is now met

# Check constant variance + linearity
plot(postcovid_diesel_poly$residuals ~ postcovid_diesel_poly$fitted.values) 
# residuals are randomly distributed around r = 0
# variance is approximately constant for all fitted values

bptest(postcovid_diesel_poly) # constant variance assumption is met


# Results of the regression model
summary(postcovid_diesel_poly) # non-sig



######## ######## 

# RQ 8.3: What if we consider all gas prices?
summary(lm(CPI.median ~ Unleaded.gas.prices + Average.diesel.prices + Average.HFCE, data = cpi_main))
# 1. All types of gas, except for regular unleaded gasoline, are significant predictors of change in inflation rate (p < 0.05)
# 2. R-squared = 0.896, meaning 89.6% of variation in inflation rate can be explained by this model
# 3. In terms of significant predictors, for every unity increase in diesel price, inflation rate decreases by 0.05
# 4. For every unit increase in HFCE, inflation rate increases by 0.08



# RQ9: Is gas price (unleaded gasoline, diesel fuel, and HFCE) in each province dependent/independent of inflation rate?
# Reference: https://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r

##### For this one, we will have to manipulate the gas_prices data frame instead (add CPI median in the data frame) #####


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



###### Correlation analysis ###### 

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
          add.params = list(color = "blue"),
          size = 1,
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
          add.params = list(color = "blue"),
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
          add.params = list(color = "blue"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Household heating fuel expense (in Cents per Litre)", ylab = "CPI median", title = "Correlation between Household heating fuel expenses and CPI median")
# From the graph, the relationship is linear, assumption 2 is met

# Spearman correlation test
cor.test(household_heating_fuel_cpi$VALUE, household_heating_fuel_cpi$CPI.median, method = "spearman", exact = FALSE)
# p-value < 0.05, meaning that there is significant correlation between HFCE and inflation rate
# H0: correlation coefficient, rho, equals to 0
# Ha: correlation coefficient, rho, does not equal to 0



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


