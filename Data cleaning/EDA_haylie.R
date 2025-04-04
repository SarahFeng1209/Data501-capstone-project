#### Load packages
library(summarytools)
library(lme4)
library(tidyverse)
library(mosaic)
library(ggplot2)

options(scipen = 999)


# Dealing with special characters in csv -- update directories when running on different machines!!
guess_encoding("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/unleaded_gas_prices_cleaned.csv")
guess_encoding("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/population_changes_abnormal_cleaned.csv")
guess_encoding("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/population_changes_regular_cleaned.csv")

##################


#### Load CSV files -- update directories when running on different machines!!
debt <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/debt_cleaned.csv", na.strings = c(" ", NA))
gas_prices <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/unleaded_gas_prices_cleaned.csv", na.strings = c(" ", NA))
household_income <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/household_income_cleaned.csv", na.strings = c(" ", NA))
housing_price_increase <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/housing_price_increase_cleaned.csv", na.strings = c(" ", NA))
population_change <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/population_change_cleaned.csv", na.strings = c(" ", NA))


##################


# Update data type of REF_DATE for all dataframes
debt$REF_DATE <- as.Date(debt$REF_DATE)
gas_prices$REF_DATE <- as.Date(gas_prices$REF_DATE)
household_income$REF_DATE <- as.Date(household_income$REF_DATE)
housing_price_increase$REF_DATE <- as.Date(housing_price_increase$REF_DATE)
# population_change$REF_DATE <- as.Date(population_change$REF_DATE)


##################


#### Descriptive

## Debt (in millions)
favstats(~Amount|Central.government.operations, data = debt)


## Unleaded gasoline price (cents per litre)

# By Province
favstats(~VALUE|Province, data = gas_prices)

# By City
favstats(~VALUE|City, data = gas_prices)


## Household income (in millions)

# By income quintile
favstats(~VALUE|Characteristics, data = household_income)

# By type of assets/liabilities
favstats(~VALUE|Wealth, data = household_income)


## Housing price increase (with the index in Dec 2016 = 100)

favstats(~VALUE|Year, data = housing_price_increase)


## Population change (in number of people)

# By Year
population_change %>% 
  group_by(REF_DATE) %>% 
  summarise(Change = mean(VALUE, na.rm = T),
            SD = sd(VALUE, na.rm = T))

# By Province
population_change %>% 
  group_by(Province) %>% 
  summarise(Change = mean(VALUE, na.rm = T),
            SD = sd(VALUE, na.rm = T))

# Considered doing city, but it's too much data...

# By Components of population growth
population_change %>% 
  group_by(Components.of.population.growth) %>% 
  summarise(Change = mean(VALUE, na.rm = T),
            SD = sd(VALUE, na.rm = T))

# By Type of area
population_change %>% 
  group_by(Type.of.area) %>% 
  summarise(Change = mean(VALUE, na.rm = T),
            SD = sd(VALUE, na.rm = T))



# By Province + Components of population growth
population_change %>% 
  group_by(Province, Components.of.population.growth) %>% 
  summarise(Change = mean(VALUE, na.rm = T),
            SD = sd(VALUE, na.rm = T))

# By Year + Components of population growth
population_change %>% 
  group_by(REF_DATE, Components.of.population.growth) %>% 
  summarise(Change = mean(VALUE, na.rm = T),
            SD = sd(VALUE, na.rm = T))


##################


#### Visualization

## Debt

# ggplot(data = debt, aes(x = REF_DATE, y = Amount, fill = Central.government.operations)) + geom_bar(position = "stack", stat = "identity")

# Not doing it this time -- March 9


## Gas price
ggplot(data = gas_prices, aes(x = Province, y = VALUE, fill = Province)) + geom_bar(stat = "summary")

mean_gas_price <- gas_prices %>% 
  group_by(REF_DATE) %>% 
  summarise(mean_gas_price = mean(VALUE, na.rm = T))


gas_prices %>% 
  filter(Province == "Canada") %>% 
  ggplot(aes(x = REF_DATE, y = VALUE)) + geom_line(col = "red") + geom_point(col = "red") + xlab("Month") + ylab("Gas Price (Cents per Litre)") + ggtitle("Line Graph of Canadian Gas Prices by Month")


## Housing price increase

# By month
ggplot(data = housing_price_increase, aes(x = REF_DATE, y = Increased.ratio.compared.to.Dec.2016)) + geom_line(col = "blue") + geom_point(col = "blue") + xlab("Month") + ylab("Increase in Housing Price Index Comparing to December 2016") + ggtitle("Line graph of Increase in Housing Price Index by Month")

# By year
ggplot(data = housing_price_increase, aes(x = Year, y = VALUE)) + geom_bar(col = "blue", fill = "blue", stat = "summary") + xlab("Year") + ylab("Average Housing Price Index") + ggtitle("Bar graph of Average Housing Price Index by Year")


## Population change
population_change$REF_DATE <- as.Date(population_change$REF_DATE)

population_change %>% 
  filter(Province == "Canada") %>% 
  ggplot(aes(x = REF_DATE,))


##################


#### Regression


## Housing price increase

# Question: does year predict increase in housing price index?

housing_price_lm <- lm(VALUE ~ Year, data = housing_price_increase)
summary(housing_price_lm)

predict(housing_price_lm, newdata = data.frame(Year=2017), interval = "confidence")

# Explanation: 
# Intercept: For Year = 0, the housing price index is -10093.
# Estimate for Year: for every unit increase in Year, there's ~5 unit increase in housing price index

# plot(VALUE ~ REF_DATE, data = housing_price_increase)


## Population change

# summary(aov(VALUE ~ Province + REF_DATE, data = population_change))

pop_change_lm <- lm(VALUE ~ Province + REF_DATE, data = population_change)
summary(pop_change_lm)

summary(lm(VALUE ~ Province, data = gas_prices))
