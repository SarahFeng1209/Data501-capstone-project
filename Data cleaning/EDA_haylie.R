#### Load packages
library(summarytools)
library(lme4)
library(tidyverse)
library(mosaic)
library(ggplot2)
library(readxl)


options(scipen = 999)


# Dealing with special characters in csv 
guess_encoding("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/unleaded_gas_prices_cleaned.csv")
guess_encoding("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/population_change_cleaned.csv")

##################


#### Load CSV files -- update directories when running on different machines!!
debt <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/debt_cleaned.csv", na.strings = c(" ", NA))
gas_prices <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/unleaded_gas_prices_cleaned.csv", na.strings = c(" ", NA))
household_income <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/household_income_cleaned.csv", na.strings = c(" ", NA))
housing_price_increase <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/housing_price_increase_cleaned.csv", na.strings = c(" ", NA))
population_change <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/population_change_cleaned.csv", na.strings = c(" ", NA))
cpi <- read_excel("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/CPI data - cleaned.xlsx", sheet = "YoY data")
diesel <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/diesel_cleaned.csv", na.strings = c(" ", NA))
household_heating_fuel <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/household_heating_fuel_cleaned.csv", na.strings = c(" ", NA))



##################


# Update data type of REF_DATE for necessary data frames
debt$REF_DATE <- as.Date(debt$REF_DATE)
gas_prices$REF_DATE <- as.Date(gas_prices$REF_DATE)
household_income$REF_DATE <- as.Date(household_income$REF_DATE)
housing_price_increase$REF_DATE <- as.Date(housing_price_increase$REF_DATE)


##################


#### Descriptive

## Debt (in millions)
favstats(~VALUE|Central.government.operations, data = debt)

debt %>% 
  filter(Central.government.operations == "A. Budgetary balance") %>% 
  favstats(~VALUE|covid, data = .)




## Unleaded gasoline price (cents per litre)

# Extract the year value from REF_DATE
gas_prices$Year <- format(gas_prices$REF_DATE, "%Y")

gas_prices %>% 
  summarise(
    mean_price = mean(VALUE, na.rm = TRUE),
    median_price = median(VALUE, na.rm = TRUE),
    sd_price = sd(VALUE, na.rm = TRUE)
  )

gas_prices %>%
  group_by(Year) %>% 
  summarise(
    mean_price = mean(VALUE, na.rm = TRUE),
    median_price = median(VALUE, na.rm = TRUE),
    sd_price = sd(VALUE, na.rm = TRUE)
  )

gas_prices %>% 
  filter(Province == "Canada") %>% 
  group_by(covid) %>% 
  summarise(
    n = n(),
    mean_price = mean(VALUE, na.rm = TRUE),
    median_price = median(VALUE, na.rm = TRUE),
    sd_price = sd(VALUE, na.rm = TRUE),
    min = min(VALUE, na.rm = TRUE),
    max = max(VALUE, na.rm = TRUE)
  )

# By Province
favstats(~VALUE|Province, data = gas_prices)

# By City
favstats(~VALUE|City, data = gas_prices)

gas_prices %>% 
  filter(Province == "Canada") %>%
  mutate(covid = factor(covid, levels = c("Pre-COVID", "COVID", "Post-COVID"))) %>% 
  favstats(~VALUE|covid, data = .)

favstats(~VALUE|covid, data = gas_prices)


## Household income (in millions)

# By income quintile
favstats(~VALUE|Characteristics, data = household_income)

# By type of assets/liabilities
favstats(~VALUE|Wealth, data = household_income)


## Housing price increase (with the index in Dec 2016 = 100)
housing_price_increase %>% 
  summarise(mean = mean(Increased.ratio.compared.to.Dec.2016),
            sd = sd(Increased.ratio.compared.to.Dec.2016))

housing_price_increase %>% 
  group_by(Year) %>% 
  summarise(mean = mean(Increased.ratio.compared.to.Dec.2016),
            sd = sd(Increased.ratio.compared.to.Dec.2016))

housing_price_increase %>% 
  mutate(covid = factor(covid, levels = c("Pre-COVID", "COVID", "Post-COVID"))) %>% 
  group_by(covid) %>% 
  summarise(n = n(),
            mean = mean(Increased.ratio.compared.to.Dec.2016),
            median = median(Increased.ratio.compared.to.Dec.2016),
            sd = sd(Increased.ratio.compared.to.Dec.2016),
            min = min(Increased.ratio.compared.to.Dec.2016),
            max = max(Increased.ratio.compared.to.Dec.2016))

favstats(~VALUE|Year, data = housing_price_increase)


## Population change (in number of people)

population_change %>% 
  filter(Province == "Canada" & Type.of.area == "Total CMA and CA") %>% 
  favstats(~VALUE|Components.of.population.growth, data = .)

population_change %>% 
  summarise(Mean = mean(VALUE, na.rm = T),
            SD = sd(VALUE, na.rm = T))

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


## CPI
cpi %>% 
  summarise(mean.CPI.median = mean(`CPI-median`, na.rm = T),
            median.CPI.median = median(`CPI-median`, na.rm = T),
            sd.CPI.median = sd(`CPI-median`, na.rm = T),
            min.CPI.median = min(`CPI-median`, na.rm = T),
            max.CPI.median = max(`CPI-median`, na.rm = T),
            
            mean.CPI.trim = mean(`CPI-trim`, na.rm = T),
            median.CPI.trim = median(`CPI-trim`, na.rm = T),
            sd.CPI.trim = sd(`CPI-trim`, na.rm = T),
            min.CPI.trim = min(`CPI-trim`, na.rm = T),
            max.CPI.trim = max(`CPI-trim`, na.rm = T),
            
            mean.CPI.common = mean(`CPI-common`, na.rm = T),
            median.CPI.common = median(`CPI-common`, na.rm = T),
            sd.CPI.common = sd(`CPI-common`, na.rm = T),
            min.CPI.common = min(`CPI-common`, na.rm = T),
            max.CPI.common = max(`CPI-common`, na.rm = T))


## Diesel
diesel %>% 
  group_by(Province) %>% 
  summarise(n = n(),
            mean = mean(VALUE, na.rm = T),
            median = median(VALUE, na.rm = T),
            sd = sd(VALUE, na.rm = T),
            min = min(VALUE, na.rm = T),
            max = max(VALUE, na.rm = T))

## Household heating fuel
favstats(~VALUE|Province, data = household_heating_fuel)


# Household income (liabilities)
household_income %>% 
  filter(Wealth == "Total liabilities") %>% 
  favstats(~VALUE|Characteristics, data = .)

# Household income (assets)
household_income %>% 
  filter(Wealth == "Total assets") %>% 
  favstats(~VALUE|Characteristics, data = .)




##################


#### Visualization

## Debt

# Overall balance

# By COVID periods
debt %>% 
  mutate(covid = factor(covid, levels = c("Pre-COVID", "COVID", "Post-COVID"))) %>% 
  filter(Central.government.operations == "A. Budgetary balance") %>% 
  ggplot(aes(x = REF_DATE, y = VALUE, group = covid, colour = covid)) + geom_hline(yintercept = 0) + geom_line() + geom_point() +
  xlab("Months") + ylab("Amount (in million dollars)") + ggtitle("Line graph of the Canadian government's budgetary balance by Month and COVID periods") +
  scale_color_manual(name = "COVID Periods", values = c("Pre-COVID" = "#619CFF", "COVID" = "#F8766D", "Post-COVID" = "#00BA38"))


## Gas price

# By province
gas_prices %>% 
  filter(Province != "Canada") %>% 
  ggplot(aes(x = Province, y = VALUE, fill = Province)) + geom_bar(stat = "summary") +
  ylab("Average Gas Price (Cents per Litre)") + ggtitle("Bar Graph of Average Gas Price by Province")

gas_prices %>% 
  filter(Province != "Canada") %>% 
  ggplot(aes(x = Province, y = VALUE, colour = Province)) + geom_boxplot() +
  ylab("Gas Price (Cents per Litre)") + ggtitle("Boxplot of Gas Prices by Province") + 
  theme(legend.position = "none")

# By national average

# Line graph of national average
gas_prices %>% 
  filter(Province == "Canada") %>% 
  ggplot(aes(x = REF_DATE, y = VALUE)) + geom_line(col = "red") + geom_point(col = "red") + 
  xlab("Month") + ylab("Average Gas Price (Cents per Litre)") + ggtitle("Line Graph of Average Canadian Gas Prices by Month")

# Line graph by COVID period, national average
gas_prices %>% 
  filter(Province == "Canada") %>% 
  mutate(covid = factor(covid, levels = c("Pre-COVID", "COVID", "Post-COVID"))) %>% 
  ggplot(aes(x = REF_DATE, y = VALUE, group = covid, colour = covid)) + geom_line() + geom_point() + 
  xlab("Month") + ylab("Gas Price (Cents per Litre)") + ggtitle("Line Graph of Canadian Gas Prices by Month and COVID Periods") + 
  scale_color_manual(name = "COVID Periods", values = c("Pre-COVID" = "#619CFF", "COVID" = "#F8766D", "Post-COVID" = "#00BA38"))

## Household income

# Reorder levels of income quintile
household_income$Characteristics <- factor(household_income$Characteristics, order = T, 
                                           levels = c("All households", "Lowest income quintile", "Second income quintile", "Third income quintile", "Fourth income quintile", "Highest income quintile"))

household_income %>% 
  filter(Characteristics == "All households") %>% 
  ggplot(aes(x = REF_DATE, y = VALUE, group = Wealth, colour = Wealth)) + geom_line() + geom_point()

# Create a data frame for ggplot background colors
household_income_bgd_col <- data.frame(
  xmin = as.Date(c("2017-10-01", "2020-04-01", "2022-10-01")),
  xmax = as.Date(c("2020-04-01", "2022-10-01", "2024-07-01")),
  covid = c("Pre-COVID", "COVID", "Post-COVID")
)

household_income %>% 
  filter(Wealth == "Net worth (wealth)" & Characteristics != "All households") %>% 
  mutate(covid = factor(covid, levels = "Pre-COVID", "COVID", "Post-COVID")) %>% 
  ggplot(aes(x = REF_DATE, y = VALUE, group = Characteristics, colour = Characteristics)) + geom_line() + geom_point() +
  geom_rect(data = household_income_bgd_col, aes(xmin = xmin, xmax = xmax, 
                ymin = -Inf, ymax = Inf, fill = covid), alpha = 0.2, inherit.aes = F) + 
  xlab("Time (3-month intervals)") + ylab("Amount") + ggtitle("Line graph of Net Worth of All Households across Time by Income Quintile and COVID Period") +
  scale_fill_manual(
    name = "COVID Period",
    values = c("Pre-COVID" = "skyblue", "COVID" = "pink", "Post-COVID" = "lightgreen"),
    breaks = c("Pre-COVID", "COVID", "Post-COVID")
  ) + 
  scale_color_discrete(name = "Income Quintile")

household_income %>% 
  filter(Wealth == "Total liabilities" & Characteristics != "All households") %>% 
  mutate(covid = factor(covid, levels = "Pre-COVID", "COVID", "Post-COVID")) %>% 
  ggplot(aes(x = REF_DATE, y = VALUE, group = Characteristics, colour = Characteristics)) + geom_line() + geom_point() +
  geom_rect(data = household_income_bgd_col, aes(xmin = xmin, xmax = xmax, 
                                                 ymin = -Inf, ymax = Inf, fill = covid), alpha = 0.2, inherit.aes = F) + 
  scale_colour_hue() +
  xlab("Time (3-month intervals)") + ylab("Amount") + ggtitle("Line graph of Total Liabilities of All Households across Time by Income Quintile and COVID Period") +
  scale_fill_manual(
    name = "COVID Period",
    values = c("Pre-COVID" = "skyblue", "COVID" = "pink", "Post-COVID" = "lightgreen"),
    breaks = c("Pre-COVID", "COVID", "Post-COVID")
  ) +
  scale_color_discrete(name = "Income Quintile")



## Housing price increase

# By month
housing_price_increase %>% 
  mutate(covid = factor(covid, levels = c("Pre-COVID", "COVID", "Post-COVID"))) %>% 
  ggplot(aes(x = REF_DATE, y = Increased.ratio.compared.to.Dec.2016, col = covid)) + geom_line() + geom_point() + 
  xlab("Month") + ylab("Increase in Housing Price Index Comparing to December 2016") + 
  ggtitle("Trend of Increase in Housing Price Index by Month") +
  scale_x_continuous(breaks = housing_price_increase$REF_DATE) +  # Ensure every label is shown
  scale_color_manual(name = "COVID Periods", values = c("Pre-COVID" = "#619CFF", "COVID" = "#F8766D", "Post-COVID" = "#00BA38")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 2))

# By year
ggplot(data = housing_price_increase, aes(x = Year, y = VALUE)) + geom_bar(col = "blue", fill = "blue", stat = "summary") + 
  xlab("Year") + ylab("Average Housing Price Index") + ggtitle("Bar graph of Average Housing Price Index by Year")


## Population change

# Nationally, by year

# Stacked bar graph
population_change %>% 
  filter(Province == "Canada") %>% 
  filter(Components.of.population.growth == "Births" | Components.of.population.growth == "Deaths" | Components.of.population.growth == "Emigrants" | Components.of.population.growth == "Immigrants") %>% 
  ggplot(aes(x = REF_DATE, y = VALUE, fill = Components.of.population.growth)) + geom_bar(position = "stack", stat = "identity") +
  xlab("Year") + ylab("Count") + ggtitle("Bar graph of Population Change in Canada over Time") +
  scale_fill_discrete(name = "Components of population change")

# Bar graph
population_change %>% 
  filter(Province == "Canada") %>% 
  filter(Components.of.population.growth == "Births" | Components.of.population.growth == "Deaths" | Components.of.population.growth == "Emigrants" | Components.of.population.growth == "Immigrants") %>% 
  ggplot(aes(x = REF_DATE, y = VALUE, fill = Components.of.population.growth)) + geom_bar(position = "dodge", stat = "identity") +
  xlab("Year") + ylab("Count") + ggtitle("Bar graph of Population Change in Canada over Time") +
  scale_fill_discrete(name = "Components of population change")

# Line graph
population_change %>% 
  filter(Province == "Canada") %>% 
  filter(Components.of.population.growth == "Births" | Components.of.population.growth == "Deaths" | Components.of.population.growth == "Emigrants" | Components.of.population.growth == "Immigrants") %>% 
  ggplot(aes(x = REF_DATE, y = VALUE, colour = Components.of.population.growth, group = Components.of.population.growth)) + geom_point(stat = "summary") + geom_line(stat = "summary") + 
  xlab("Year") + ylab("Count") + ggtitle("Line graph of Population Change in Canada over Time") +
  scale_color_discrete(name = "Components of population change")


## diesel
diesel %>% 
  ggplot(aes(x = Province, y = VALUE, colour = Province)) + geom_boxplot() +
  xlab("Province") + ylab("Diesel Fuel Price (in Cents per Litre)") + ggtitle("Boxplot of Diesel Fuel Prices by Province") + 
  theme(legend.position = "none")

diesel %>% 
  mutate(covid = factor(covid, levels = c("Pre-COVID", "COVID", "Post-COVID"))) %>% 
  ggplot(aes(x = REF_DATE, y = VALUE, group = covid, colour = covid)) + geom_line() + geom_point() + 
  xlab("Month") + ylab("Gas Price (Cents per Litre)") + ggtitle("Line Graph of Canadian Gas Prices by Month and COVID Periods") + 
  scale_color_manual(name = "COVID Periods", values = c("Pre-COVID" = "#619CFF", "COVID" = "#F8766D", "Post-COVID" = "#00BA38"))


## HFCE
HFCE %>% 
  ggplot(aes(x = Province, y = VALUE, colour = Province)) + geom_boxplot() +
  xlab("Province") + ylab("Household Heating Fuel Prices (in Cents per Litre)") + ggtitle("Boxplot of Household Heating Fuel Prices by Province") + 
  theme(legend.position = "none")


##################


#### Regression

## Gas price

summary(aov(VALUE ~ covid + Province, data = gas_prices))

summary(aov(VALUE ~ REF_DATE*Province, data = gas_prices))


## Housing price increase

#### Question: does year predict increase in housing price index?

housing_price_lm <- lm(VALUE ~ Year, data = housing_price_increase)
summary(housing_price_lm)

# Explanation: 
# Intercept: For Year = 0, the housing price index is -10093.
# Estimate for Year: for every unit increase in Year, there's ~5 unit increase in housing price index


#### RQ: What is the relationship between housing prices and inflation rate?




# plot(VALUE ~ REF_DATE, data = housing_price_increase)


## Population change

# summary(aov(VALUE ~ Province + REF_DATE, data = population_change))

pop_change_lm <- lm(VALUE ~ Province + REF_DATE, data = population_change)
summary(pop_change_lm)

summary(lm(VALUE ~ Province, data = population_change))

summary(lm(VALUE ~ Province, data = gas_prices))

#### RQ: What is the relationship between population size and inflation rate?




## Federal balance sheet
# federal_balance_sheet$REF_DATE <- as_date(ym(federal_balance_sheet$REF_DATE))
# 
# summary(lm(Liabilities ~ Revenue + Social.benefits, data = federal_balance_sheet))
# 
# summary(lm(Liabilities ~ Revenue + Social.benefits + REF_DATE, data = federal_balance_sheet))


## Diesel price


# summary(lm(VALUE ~ Province, data = diesel_cleaned))
# summary(lm(VALUE ~ REF_DATE, data = diesel_cleaned))
# summary(lm(VALUE ~ Province + REF_DATE, data = diesel_cleaned))
# 
# 
# summary(aov(lm(VALUE ~ Province, data = diesel_cleaned)))
