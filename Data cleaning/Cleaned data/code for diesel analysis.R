#Statistic of each diesel+fuel at self sevrice

file_path <- "C:\\Users\\fengm\\Downloads\\cleaned Diesel fuel at self service filling stations.xlsx"

#rename the file name as "fuel"
fuel <- read_excel(file_path)
fuel

# To read the list of Canadian cities to categorize
canadian_cities <- c(
  "St. John's, Newfoundland and Labrador",
  "Charlottetown and Summerside, Prince Edward Island",
  "Halifax, Nova Scotia",
  "Saint John, New Brunswick",
  "Québec, Quebec",
  "Montréal, Quebec",
  "Ottawa-Gatineau, Ontario part, Ontario/Quebec",
  "Toronto, Ontario",
  "Winnipeg, Manitoba",
  "Regina, Saskatchewan",
  "Saskatoon, Saskatchewan",
  "Edmonton, Alberta",
  "Calgary, Alberta",
  "Vancouver, British Columbia",
  "Victoria, British Columbia",
  "Whitehorse, Yukon",
  "Yellowknife, Northwest Territories"
)

# Filter the dataset to include only the specified Canadian cities
fuel_canada <- fuel %>% filter(GEO %in% canadian_cities)

# Group by GEO and compute mean,median & sd
geo_stats <- fuel_canada %>%
  group_by(GEO) %>%
  summarise(
    mean_price = mean(VALUE, na.rm = TRUE),
    median_price = median(VALUE, na.rm = TRUE),
    sd_price = sd(VALUE, na.rm = TRUE)
  )
print(geo_stats)

ggplot(geo_stats, aes(x = reorder(GEO, mean_price), y = mean_price, fill = GEO)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the bar chart for better readability
  labs(title = "Mean Diesel Price by Canadian City",
       x = "City",
       y = "Mean Diesel Price") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend for cleaner visualization

# Plot median diesel price for each Canadian city
ggplot(geo_stats, aes(x = reorder(GEO, median_price), y = median_price, fill = GEO)) +
  geom_bar(stat = "identity", fill = "pink") +
  coord_flip() +  # Flip the bar chart for better readability
  labs(title = "Median Diesel Price by Canadian City",
       x = "City",
       y = "Median Diesel Price") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend for cleaner visualization

# Plot standard deviation of diesel price for each Canadian city
ggplot(geo_stats, aes(x = reorder(GEO, sd_price), y = sd_price, fill = GEO)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +  # Flip the bar chart for better readability
  labs(title = "Standard Deviation of Diesel Price by Canadian City",
       x = "City",
       y = "Standard Deviation of Diesel Price") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend for cleaner visualization

# Plot line graph for Mean Diesel Prices for each GEO
ggplot(geo_stats, aes(x = GEO, y = mean_price, group = 1)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(color = "red", size = 3) +
  coord_flip() +  # Flip for better readability
  labs(title = "Mean Diesel Price by Canadian City",
       x = "City",
       y = "Mean Diesel Price") +
  theme_minimal()


