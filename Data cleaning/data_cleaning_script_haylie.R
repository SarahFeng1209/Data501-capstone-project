#### Load packages
library(readxl)
library(summarytools)
library(lme4)
library(tidyverse)
library(mosaic)


# Dealing with special characters in csv -- update directories when running on different machines!!
guess_encoding("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/To clean/gas_prices.csv")
guess_encoding("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/To clean/population_changes_abnormal.csv")
guess_encoding("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/To clean/population_changes_regular.csv")
guess_encoding("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/To clean/diesel.csv")
guess_encoding("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/To clean/household_heating_fuel.csv")

##################


#### Load CSV files -- update directories when running on different machines!!
debt <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/To clean/debt.csv", na.strings = c(" ", NA))
gas_prices <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/To clean/gas_prices.csv", fileEncoding = "ISO-8859-1", na.strings = c(" ", NA))
household_income <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/To clean/household_income.csv", na.strings = c(" ", NA))
housing_price_increase <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/To clean/housing_price_increase.csv", na.strings = c(" ", NA))
population_changes_abnormal <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/To clean/population_changes_abnormal.csv", fileEncoding = "ISO-8859-1", na.strings = c(" ", NA))
population_changes_regular <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/To clean/population_changes_regular.csv", fileEncoding = "ISO-8859-1", na.strings = c(" ", NA))
diesel <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/To clean/diesel.csv", fileEncoding = "ISO-8859-1", na.strings = c(" ", NA))
household_heating_fuel <- read.csv("C:/Users/User/Desktop/UofC/W25/DATA 501/Data/To clean/household_heating_fuel.csv", fileEncoding = "ISO-8859-1", na.strings = c(" ", NA))


##################


#### For debt ####

# Clean out unnecessary columns
debt_cleaned <- debt %>% 
  select(-c(GEO, DGUID, UOM, UOM_ID, SCALAR_FACTOR, COORDINATE))

# Fill blank cells with NAs
debt_cleaned[debt_cleaned == ""] <- NA

# Change data type of REF_DATE to date for easier manipulation
debt_cleaned$REF_DATE <- as_date(ym(debt_cleaned$REF_DATE))
  
# Update names of Central.government.operations
debt_cleaned <- debt_cleaned %>% 
  mutate(Central.government.operations = case_when(
    Central.government.operations == "A. Budgetary balance, deficit (-) / surplus (+), (B - C)" ~ "A. Budgetary balance",
    Central.government.operations == "E. Financial source (+) / requirement (-), (A + D)" ~ "E. Financial source",
    Central.government.operations == "G. Net change in cash balances, (E + F)" ~ "G. Net change in cash balances",
    TRUE ~ Central.government.operations
  ))

## Change the labeling of +/- in VALUE
debt_cleaned <- debt_cleaned %>% 
  mutate(VALUE = case_when(
    VALUE == " $-   " ~ NA,
    TRUE ~ VALUE
  ))

# Replace parentheses with -
debt_cleaned$VALUE <- gsub("\\((.*?)\\)", "-\\1", debt_cleaned$VALUE)

# Remove $ and ,
debt_cleaned$VALUE <- gsub("[$,]", "", debt_cleaned$VALUE)

# Change data type
debt_cleaned$VALUE <- as.numeric(debt_cleaned$VALUE)

# Rename the column
debt_cleaned <- debt_cleaned %>% rename(Amount = VALUE)

# Add COVID periods
debt_cleaned <- debt_cleaned %>% 
  mutate(covid = case_when(
    REF_DATE < "2020-03-01" ~ "Pre-COVID",
    REF_DATE > "2022-07-01" ~ "Post-COVID",
    TRUE ~ "COVID"
  ))

  
##################


#### For gas_prices #### (Unleaded Gasoline)

# Clean out unnecessary columns
gas_prices_cleaned <- gas_prices %>% 
  select(-c(DGUID, Type.of.fuel, UOM, UOM_ID, SCALAR_FACTOR, SCALAR_ID, VECTOR, COORDINATE))

# Type of fuel = Regular unleaded gasoline at self service filling stations (for all)
# Unit of Measure = Cents per Litre

# Change data type of REF_DATE to date for easier manipulation
gas_prices_cleaned$REF_DATE <- as_date(ym(gas_prices_cleaned$REF_DATE))

gas_prices_cleaned <- gas_prices_cleaned %>% 
  mutate(GEO = case_when(
    # Update names of GEO
    GEO == "Ottawa-Gatineau, Ontario part, Ontario/Quebec" ~ "Ottawa-Gatineau, Ontario",
    GEO == "Canada" ~ "Canada, Canada",
    TRUE ~ GEO
  )) %>% 
  # Split character strings into City and Province
  separate(GEO, into = c('City', 'Province'), sep = ", ")

# Shorten the names of provinces
gas_prices_cleaned <- gas_prices_cleaned %>%
  mutate(Province = case_when(
    Province == "Alberta" ~ "AB",
    Province == "British Columbia" ~ "BC",
    Province == "Manitoba" ~ "MB",
    Province == "New Brunswick" ~ "NB",
    Province == "Newfoundland and Labrador" ~ "NL",
    Province == "Northwest Territories" ~ "NT",
    Province == "Nova Scotia" ~ "NS",
    Province == "Nunavut" ~ "NU",
    Province == "Ontario" ~ "ON",
    Province == "Prince Edward Island" ~ "PE",
    Province == "Quebec" ~ "QC",
    Province == "Saskatchewan" ~ "SK",
    Province == "Yukon" ~ "YT",
    TRUE ~ Province
  ))

# Add COVID periods
gas_prices_cleaned <- gas_prices_cleaned %>% 
  mutate(covid = case_when(
    REF_DATE < "2020-03-01" ~ "Pre-COVID",
    REF_DATE > "2022-07-01" ~ "Post-COVID",
    TRUE ~ "COVID"
  ))


##################


# For household_income

# Clean out unnecessary columns
household_income_cleaned <- household_income %>% 
  select(-c(GEO:Statistics, UOM:COORDINATE, STATUS:DECIMALS))

# Fill blank cells with NAs
household_income_cleaned[household_income_cleaned == ""] <- NA

# Drop subheader rows
household_income_cleaned <- household_income_cleaned %>% filter(!is.na(Wealth))

# Change data type of REF_DATE to date for easier manipulation
household_income_cleaned$REF_DATE <- as_date(ym(household_income_cleaned$REF_DATE))

# Add COVID periods
household_income_cleaned <- household_income_cleaned %>% 
  mutate(covid = case_when(
    REF_DATE < "2020-03-01" ~ "Pre-COVID",
    REF_DATE > "2022-07-01" ~ "Post-COVID",
    TRUE ~ "COVID"
  ))


##################


# For housing_price_increase (Note: Houses only)

# Clean out unnecessary columns
housing_price_increase_cleaned <- housing_price_increase %>% 
  select(-c(GEO:COORDINATE, X100))

# Change data type of REF_DATE to date for easier manipulation
housing_price_increase_cleaned$REF_DATE <- as_date(ym(housing_price_increase_cleaned$REF_DATE))

# Create a new variable "Year"
housing_price_increase_cleaned$Year <- format(housing_price_increase_cleaned$REF_DATE, "%Y")

# Add COVID periods
housing_price_increase_cleaned <- housing_price_increase_cleaned %>% 
  mutate(covid = case_when(
    REF_DATE < "2020-03-01" ~ "Pre-COVID",
    REF_DATE > "2022-07-01" ~ "Post-COVID",
    TRUE ~ "COVID"
  ))


##################


# For population_changes_abnormal

# Clean out unnecessary columns
population_changes_abnormal_cleaned <- population_changes_abnormal %>% 
  select(-c(DGUID, Gender:COORDINATE))

# Change Names of GEO
population_changes_abnormal_cleaned <- population_changes_abnormal_cleaned %>% 
  mutate(GEO = case_when(
    grepl("All areas outside census metropolitan areas and census agglomerations", GEO) ~ gsub("All areas outside census metropolitan areas and census agglomerations", "Total Small Towns", GEO),
    grepl("Area outside census metropolitan areas and census agglomerations", GEO) ~ gsub("Area outside census metropolitan areas and census agglomerations", "Small Towns", GEO),
    grepl("All census metropolitan areas and census agglomerations", GEO) ~ gsub("All census metropolitan areas and census agglomerations", "Total CMA and CA", GEO),
    grepl("All census agglomerations", GEO) ~ gsub("All census agglomerations", "Total CA", GEO),
    grepl("All census metropolitan areas", GEO) ~ gsub("All census metropolitan areas", "Total CMA", GEO),
    TRUE ~ GEO  # Keep the original value if no condition is met
  )) %>% 
  mutate(GEO = case_when(
    GEO == "Campbellton (CA), New Brunswick part, New Brunswick" ~ "Campbellton (CA), New Brunswick",
    # GEO == "Campbellton (CA), New Brunswick/Quebec" ~ "Campbellton (CA), New Brunswick",
    GEO == "Campbellton (CA), Quebec part, Quebec" ~ "Campbellton (CA), Quebec",
    GEO == "Hawkesbury (CA), Ontario part, Ontario" ~ "Hawkesbury (CA), Ontario",
    # GEO == "Hawkesbury (CA), Ontario/Quebec" ~ "Hawkesbury (CA), Ontario",
    GEO == "Hawkesbury (CA), Quebec part, Quebec" ~ "Hawkesbury (CA), Quebec",
    GEO == "Lloydminster (CA), Alberta part, Alberta" ~ "Lloydminster (CA), Alberta",
    # GEO == "Lloydminster (CA), Alberta/Saskatchewan" ~ "Lloydminster (CA), Alberta",
    GEO == "Lloydminster (CA), Saskatchewan part, Saskatchewan" ~ "Lloydminster (CA), Saskatchewan",
    GEO == "Ottawa - Gatineau (CMA), Ontario part, Ontario" ~ "Ottawa - Gatineau (CMA), Ontario",
    # GEO == "Ottawa - Gatineau (CMA), Ontario/Quebec" ~ "Ottawa - Gatineau (CMA), Ontario",
    GEO == "Ottawa - Gatineau (CMA), Quebec part, Quebec" ~ "Ottawa - Gatineau (CMA), Quebec",
    TRUE ~ GEO
  )) %>% 
  # Split character strings into City and Province
  separate(GEO, c('City', 'Province'), sep = ", ")

# Determine the type of area examined by the value in GEO
population_changes_abnormal_cleaned$Type.of.area <- gsub(".*\\((.*?)\\).*", "\\1", population_changes_abnormal_cleaned$City)

# Update the City names in City column
population_changes_abnormal_cleaned <- population_changes_abnormal_cleaned %>% 
  mutate(City = case_when(
    Province == "Canada" ~ "Canada",
    TRUE ~ City
  ))
  
# Remove the string in the brackets in the City column
population_changes_abnormal_cleaned <- population_changes_abnormal_cleaned %>% 
  mutate(City = gsub("\\s*\\(.*\\)", "", City))

# Shorten the names of provinces
population_changes_abnormal_cleaned <- population_changes_abnormal_cleaned %>%
  mutate(Province = case_when(
    Province == "Alberta" ~ "AB",
    Province == "British Columbia" ~ "BC",
    Province == "Manitoba" ~ "MB",
    Province == "New Brunswick" ~ "NB",
    Province == "Newfoundland and Labrador" ~ "NL",
    Province == "Northwest Territories" ~ "NT",
    Province == "Nova Scotia" ~ "NS",
    Province == "Nunavut" ~ "NU",
    Province == "Ontario" ~ "ON",
    Province == "Prince Edward Island" ~ "PE",
    Province == "Quebec" ~ "QC",
    Province == "Saskatchewan" ~ "SK",
    Province == "Yukon" ~ "YT",
    TRUE ~ Province
  ))

# Add COVID periods
###### NOTE: each year/year period starts from July 1 to June 30 !!!!! ######
###### So we will define 2020/2021 and 2021/2022 as our COVID periods
population_changes_abnormal_cleaned <- population_changes_abnormal_cleaned %>% 
  mutate(covid = case_when(
    REF_DATE == "2017/2018" | REF_DATE == "2018/2019" | REF_DATE == "2019/2020" ~ "Pre-COVID",
    REF_DATE == "2020/2021" | REF_DATE == "2021/2022" ~ "COVID",
    TRUE ~ "Post-COVID"
  ))


##################


# For population_changes_abnormal

# Clean out unnecessary columns
population_changes_regular_cleaned <- population_changes_regular %>% 
  select(-c(DGUID, Gender:COORDINATE))

# Change Names of GEO
population_changes_regular_cleaned <- population_changes_regular_cleaned %>% 
  mutate(GEO = case_when(
    grepl("All areas outside census metropolitan areas and census agglomerations", GEO) ~ gsub("All areas outside census metropolitan areas and census agglomerations", "Total Small Towns", GEO),
    grepl("Area outside census metropolitan areas and census agglomerations", GEO) ~ gsub("Area outside census metropolitan areas and census agglomerations", "Small Towns", GEO),
    grepl("All census metropolitan areas and census agglomerations", GEO) ~ gsub("All census metropolitan areas and census agglomerations", "Total CMA and CA", GEO),
    grepl("All census agglomerations", GEO) ~ gsub("All census agglomerations", "Total CA", GEO),
    grepl("All census metropolitan areas", GEO) ~ gsub("All census metropolitan areas", "Total CMA", GEO),
    TRUE ~ GEO  # Keep the original value if no condition is met
  )) %>% 
  mutate(GEO = case_when(
    GEO == "Campbellton (CA), New Brunswick part, New Brunswick" ~ "Campbellton (CA), New Brunswick",
    # GEO == "Campbellton (CA), New Brunswick/Quebec" ~ "Campbellton (CA), New Brunswick",
    GEO == "Campbellton (CA), Quebec part, Quebec" ~ "Campbellton (CA), Quebec",
    GEO == "Hawkesbury (CA), Ontario part, Ontario" ~ "Hawkesbury (CA), Ontario",
    # GEO == "Hawkesbury (CA), Ontario/Quebec" ~ "Hawkesbury (CA), Ontario",
    GEO == "Hawkesbury (CA), Quebec part, Quebec" ~ "Hawkesbury (CA), Quebec",
    GEO == "Lloydminster (CA), Alberta part, Alberta" ~ "Lloydminster (CA), Alberta",
    # GEO == "Lloydminster (CA), Alberta/Saskatchewan" ~ "Lloydminster (CA), Alberta",
    GEO == "Lloydminster (CA), Saskatchewan part, Saskatchewan" ~ "Lloydminster (CA), Saskatchewan",
    GEO == "Ottawa - Gatineau (CMA), Ontario part, Ontario" ~ "Ottawa - Gatineau (CMA), Ontario",
    # GEO == "Ottawa - Gatineau (CMA), Ontario/Quebec" ~ "Ottawa - Gatineau (CMA), Ontario",
    GEO == "Ottawa - Gatineau (CMA), Quebec part, Quebec" ~ "Ottawa - Gatineau (CMA), Quebec",
    TRUE ~ GEO
  )) %>% 
  # Split character strings into City and Province
  separate(GEO, c('City', 'Province'), sep = ", ")

# Determine the type of area examined by the value in GEO
population_changes_regular_cleaned$Type.of.area <- gsub(".*\\((.*?)\\).*", "\\1", population_changes_regular_cleaned$City)

# Update the City names in City column
population_changes_regular_cleaned <- population_changes_regular_cleaned %>% 
  mutate(City = case_when(
    Province == "Canada" ~ "Canada",
    TRUE ~ City
  ))

# Remove the string in the brackets in the City column
population_changes_regular_cleaned <- population_changes_regular_cleaned %>% 
  mutate(City = gsub("\\s*\\(.*\\)", "", City))

# Shorten the names of provinces
population_changes_regular_cleaned <- population_changes_regular_cleaned %>%
  mutate(Province = case_when(
    Province == "Alberta" ~ "AB",
    Province == "British Columbia" ~ "BC",
    Province == "Manitoba" ~ "MB",
    Province == "New Brunswick" ~ "NB",
    Province == "Newfoundland and Labrador" ~ "NL",
    Province == "Northwest Territories" ~ "NT",
    Province == "Nova Scotia" ~ "NS",
    Province == "Nunavut" ~ "NU",
    Province == "Ontario" ~ "ON",
    Province == "Prince Edward Island" ~ "PE",
    Province == "Quebec" ~ "QC",
    Province == "Saskatchewan" ~ "SK",
    Province == "Yukon" ~ "YT",
    TRUE ~ Province
  ))

# Add COVID periods
###### NOTE: each year/year period starts from July 1 to June 30 !!!!! ######
###### So we will define 2020/2021 and 2021/2022 as our COVID periods
population_changes_regular_cleaned <- population_changes_regular_cleaned %>% 
  mutate(covid = case_when(
    REF_DATE == "2017/2018" | REF_DATE == "2018/2019" | REF_DATE == "2019/2020" ~ "Pre-COVID",
    REF_DATE == "2020/2021" | REF_DATE == "2021/2022" ~ "COVID",
    TRUE ~ "Post-COVID"
  ))


##################


## Merge the 2 population data frames
population_change_cleaned <- bind_rows(population_changes_regular_cleaned, population_changes_abnormal_cleaned)


##################


#### For diesel ####

# Clean out unnecessary columns
diesel_cleaned <- diesel %>% 
  dplyr::select(-c(X:X.6))

# Type of fuel = diesel
# Unit of Measure = Cents per Litre

# Change data type of REF_DATE to date for easier manipulation
diesel_cleaned$REF_DATE <- as_date(ym(diesel_cleaned$REF_DATE))

diesel_cleaned <- diesel_cleaned %>% 
  mutate(GEO = case_when(
    # Update names of GEO
    GEO == "Ottawa-Gatineau, Ontario part, Ontario/Quebec" ~ "Ottawa-Gatineau, Ontario",
    # GEO == "Canada" ~ "Canada, Canada",  # National averages are not available for diesel prices
    TRUE ~ GEO
  )) %>% 
  # Split character strings into City and Province
  separate(GEO, into = c('City', 'Province'), sep = ", ")

# Shorten the names of provinces
diesel_cleaned <- diesel_cleaned %>%
  mutate(Province = case_when(
    Province == "Alberta" ~ "AB",
    Province == "British Columbia" ~ "BC",
    Province == "Manitoba" ~ "MB",
    Province == "New Brunswick" ~ "NB",
    Province == "Newfoundland and Labrador" ~ "NL",
    Province == "Northwest Territories" ~ "NT",
    Province == "Nova Scotia" ~ "NS",
    Province == "Nunavut" ~ "NU",
    Province == "Ontario" ~ "ON",
    Province == "Prince Edward Island" ~ "PE",
    Province == "Quebec" ~ "QC",
    Province == "Saskatchewan" ~ "SK",
    Province == "Yukon" ~ "YT",
    TRUE ~ Province
  ))

# Add COVID periods
diesel_cleaned <- diesel_cleaned %>% 
  mutate(covid = case_when(
    REF_DATE < "2020-03-01" ~ "Pre-COVID",
    REF_DATE > "2022-07-01" ~ "Post-COVID",
    TRUE ~ "COVID"
  ))


##################


#### For household heating fuel ####

# Clean out unnecessary columns
household_heating_fuel_cleaned <- household_heating_fuel %>% 
  dplyr::select(-c(Type.of.fuel, UOM))

# Unit of Measure = Cents per Litre

# Change data type of REF_DATE to date for easier manipulation
household_heating_fuel_cleaned$REF_DATE <- as_date(ym(household_heating_fuel_cleaned$REF_DATE))

household_heating_fuel_cleaned <- household_heating_fuel_cleaned %>% 
  mutate(GEO = case_when(
    # Update names of GEO
    GEO == "Ottawa-Gatineau, Ontario part, Ontario/Quebec" ~ "Ottawa-Gatineau, Ontario",
    # GEO == "Canada" ~ "Canada, Canada",  # National averages are not available for household heating fuel prices
    TRUE ~ GEO
  )) %>% 
  # Split character strings into City and Province
  separate(GEO, into = c('City', 'Province'), sep = ", ")

# Shorten the names of provinces
household_heating_fuel_cleaned <- household_heating_fuel_cleaned %>%
  mutate(Province = case_when(
    Province == "Alberta" ~ "AB",
    Province == "British Columbia" ~ "BC",
    Province == "Manitoba" ~ "MB",
    Province == "New Brunswick" ~ "NB",
    Province == "Newfoundland and Labrador" ~ "NL",
    Province == "Northwest Territories" ~ "NT",
    Province == "Nova Scotia" ~ "NS",
    Province == "Nunavut" ~ "NU",
    Province == "Ontario" ~ "ON",
    Province == "Prince Edward Island" ~ "PE",
    Province == "Quebec" ~ "QC",
    Province == "Saskatchewan" ~ "SK",
    Province == "Yukon" ~ "YT",
    TRUE ~ Province
  ))

# Add COVID periods
household_heating_fuel_cleaned <- household_heating_fuel_cleaned %>% 
  mutate(covid = case_when(
    REF_DATE < "2020-03-01" ~ "Pre-COVID",
    REF_DATE > "2022-07-01" ~ "Post-COVID",
    TRUE ~ "COVID"
  ))


##################


#### Export csv files -- update directories when running on different machines!!
write.csv(debt_cleaned, "C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/debt_cleaned.csv")
write.csv(gas_prices_cleaned, "C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/unleaded_gas_prices_cleaned.csv")
write.csv(household_income_cleaned, "C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/household_income_cleaned.csv")
write.csv(housing_price_increase_cleaned, "C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/housing_price_increase_cleaned.csv")
write.csv(population_change_cleaned, "C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/population_change_cleaned.csv")
write.csv(diesel_cleaned, "C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/diesel_cleaned.csv")
write.csv(household_heating_fuel_cleaned, "C:/Users/User/Desktop/UofC/W25/DATA 501/Data/Cleaned data/household_heating_fuel_cleaned.csv")
