# STEP 1: Install & Load Packages

packages <- c("rvest", "dplyr", "stringr", "tidyr", "ggplot2", "janitor", "caret")

new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(rvest)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(janitor)
library(caret)

cat("✅ Step 1 complete: Packages installed and loaded successfully!\n")



# STEP 2: Scrape GDP Table from Wikipedia

gdp_url <- "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)"

# Read the HTML
gdp_page <- read_html(gdp_url)

# Extract ALL tables from the page
gdp_tables <- gdp_page %>% html_table(fill = TRUE)

# Check how many tables were found
length(gdp_tables)

# View the first few tables to identify the correct one
head(gdp_tables[[1]])
head(gdp_tables[[2]])
head(gdp_tables[[3]])


# STEP 3: Clean GDP Table (IMF column)

gdp_raw <- gdp_tables[[3]]

# Clean column names nicely
gdp_raw <- gdp_raw %>% clean_names()

# Check column names
names(gdp_raw)

# Rename important columns for clarity
gdp_clean <- gdp_raw %>%
  rename(
    country = country_territory,
    gdp_imf = imf_2025_6
  ) %>%
  # Remove footnote text like [n 1], [6], etc. from country names
  mutate(
    country = str_replace_all(country, "\\[.*?\\]", ""),
    country = str_trim(country)
  ) %>%
  # Remove commas and convert GDP to numeric
  mutate(
    gdp_imf = str_replace_all(gdp_imf, ",", ""),
    gdp_imf = as.numeric(gdp_imf)
  )

# Preview results
head(gdp_clean, 10)

# Check missing values
sum(is.na(gdp_clean$gdp_imf))

# Quick summary
summary(gdp_clean$gdp_imf)



# STEP 3.1: Improved GDP Cleaning (Fix NA issue)

gdp_clean <- gdp_raw %>%
  rename(
    country = country_territory,
    gdp_imf = imf_2025_6
  ) %>%
  mutate(
    # clean country names
    country = str_replace_all(country, "\\[.*?\\]", ""),
    country = str_trim(country),
    
    # remove commas and keep only digits in GDP
    gdp_imf = str_replace_all(gdp_imf, ",", ""),
    gdp_imf = str_extract(gdp_imf, "\\d+"),
    gdp_imf = as.numeric(gdp_imf)
  )

# Check how many NAs remain
sum(is.na(gdp_clean$gdp_imf))

# See which countries have missing GDP
gdp_clean %>% filter(is.na(gdp_imf)) %>% head(10)



# STEP 3.2: Final GDP Dataset (Remove missing GDP)

gdp_final <- gdp_clean %>%
  filter(!is.na(gdp_imf)) %>%
  select(country, gdp_imf)

# Preview
head(gdp_final, 10)
nrow(gdp_final)



# STEP 4: Scrape Population Table from Wikipedia


pop_url <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"

# Read HTML
pop_page <- read_html(pop_url)

# Extract all tables
pop_tables <- pop_page %>% html_table(fill = TRUE)

# Check how many tables found
length(pop_tables)

# Preview first 3 tables to find the correct population table
head(pop_tables[[1]])
head(pop_tables[[2]])
head(pop_tables[[3]])


# STEP 5: Clean Population Table

pop_raw <- pop_tables[[1]]

# Clean column names
pop_raw <- pop_raw %>% clean_names()

# Check column names
names(pop_raw)

# Create clean population dataset
pop_clean <- pop_raw %>%
  rename(
    country = location,
    population = population
  ) %>%
  mutate(
    # Remove footnotes like [b], [c]
    country = str_replace_all(country, "\\[.*?\\]", ""),
    country = str_trim(country),
    
    # Remove commas, keep digits only, convert to numeric
    population = str_replace_all(population, ",", ""),
    population = str_extract(population, "\\d+"),
    population = as.numeric(population)
  )

# Check missing values
sum(is.na(pop_clean$population))

# Keep final usable rows
pop_final <- pop_clean %>%
  filter(!is.na(population)) %>%
  select(country, population)

# Preview
head(pop_final, 10)
nrow(pop_final)



# STEP 6: Merge GDP + Population

# Remove "World" row from both datasets
gdp_final2 <- gdp_final %>% filter(country != "World")
pop_final2 <- pop_final %>% filter(country != "World")

# Merge using country name
data_merged1 <- inner_join(gdp_final2, pop_final2, by = "country")

# Check merged data
head(data_merged1, 10)
nrow(data_merged1)

# Check which countries did NOT match (GDP missing but population exists)
missing_gdp_countries <- anti_join(pop_final2, gdp_final2, by = "country")

# Check which countries did NOT match (Population missing but GDP exists)
missing_pop_countries <- anti_join(gdp_final2, pop_final2, by = "country")

# How many mismatches?
nrow(missing_gdp_countries)
nrow(missing_pop_countries)

# View examples of mismatched country names
head(missing_gdp_countries, 10)
head(missing_pop_countries, 10)




# STEP 7: Fix Country Name Mismatches

# Fix population country names to match GDP country names
pop_final_fixed <- pop_final2 %>%
  mutate(country = case_when(
    country == "Democratic Republic of the Congo" ~ "DR Congo",
    country == "Republic of the Congo" ~ "Congo",
    country == "Hong Kong (China)" ~ "Hong Kong",
    country == "Puerto Rico (US)" ~ "Puerto Rico",
    country == "Macau (China)" ~ "Macau",
    TRUE ~ country
  ))

# Merge again after fixing names
data_merged2 <- inner_join(gdp_final2, pop_final_fixed, by = "country")

# Check new row count
nrow(data_merged2)

# Show first rows
head(data_merged2, 10)



# STEP 8A: Scrape HDI Table from Wikipedia


hdi_url <- "https://en.wikipedia.org/wiki/List_of_countries_by_Human_Development_Index"

# Read page
hdi_page <- read_html(hdi_url)

# Extract all tables
hdi_tables <- hdi_page %>% html_table(fill = TRUE)

# Check how many tables found
length(hdi_tables)

# Preview first few tables
head(hdi_tables[[1]])
head(hdi_tables[[2]])
head(hdi_tables[[3]])


# STEP 8B: Clean HDI Table


hdi_raw <- hdi_tables[[2]]
hdi_raw <- hdi_raw %>% clean_names()

# Check columns
names(hdi_raw)

# Clean HDI dataset
hdi_clean <- hdi_raw %>%
  rename(
    country = country_or_territory,
    hdi = hdi_value
  ) %>%
  mutate(
    # Remove footnotes if any
    country = str_replace_all(country, "\\[.*?\\]", ""),
    country = str_trim(country),
    
    # Ensure numeric
    hdi = as.numeric(hdi)
  )

# Remove missing HDI values if any
hdi_final <- hdi_clean %>%
  filter(!is.na(hdi)) %>%
  select(country, hdi)

# Preview
head(hdi_final, 10)
nrow(hdi_final)

# Check any missing HDI
sum(is.na(hdi_final$hdi))
summary(hdi_final$hdi)



# STEP 9: Merge HDI with GDP + Population

# Merge all into one dataset
final_data <- inner_join(data_merged2, hdi_final, by = "country")

# Check size
nrow(final_data)

# Preview
head(final_data, 10)

# Check missing values
colSums(is.na(final_data))

# Summary
summary(final_data)



# STEP 10: Feature Engineering + HDI Category


final_data2 <- final_data %>%
  mutate(
    # GDP per capita (GDP is in millions? Actually values are in USD millions on Wikipedia)
    # We'll compute using the same unit consistently
    gdp_per_capita = (gdp_imf * 1e6) / population,
    
    # Log transformations (reduce skew)
    log_gdp = log(gdp_imf),
    log_population = log(population),
    log_gdp_per_capita = log(gdp_per_capita),
    
    # HDI Category (3-class classification)
    hdi_category = case_when(
      hdi < 0.55 ~ "Low",
      hdi < 0.70 ~ "Medium",
      TRUE ~ "High"
    )
  )

# Check the updated dataset
head(final_data2, 10)

# Check class distribution (important for report)
table(final_data2$hdi_category)

# Basic summary of engineered features
summary(final_data2$gdp_per_capita)
summary(final_data2$log_gdp_per_capita)



# STEP 11: Exploratory Data Analysis (EDA)


# 1) Scatter plot: GDP per capita vs HDI
ggplot(final_data2, aes(x = gdp_per_capita, y = hdi)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  labs(
    title = "GDP per Capita vs HDI",
    x = "GDP per Capita (log scale)",
    y = "HDI"
  ) +
  theme_minimal()

# 2) Boxplot: GDP per capita by HDI Category
ggplot(final_data2, aes(x = hdi_category, y = gdp_per_capita)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(
    title = "GDP per Capita by HDI Category",
    x = "HDI Category",
    y = "GDP per Capita (log scale)"
  ) +
  theme_minimal()

# 3) Correlation among numeric variables
num_data <- final_data2 %>%
  select(gdp_imf, population, gdp_per_capita, hdi, log_gdp, log_population, log_gdp_per_capita)

cor_matrix <- cor(num_data)

print(cor_matrix)



# STEP 12: Train/Test Split + Baseline Model


set.seed(123)

# Convert target into factor
final_data2$hdi_category <- as.factor(final_data2$hdi_category)

# Stratified Train/Test split (70/30)
train_index <- createDataPartition(final_data2$hdi_category, p = 0.7, list = FALSE)

train_data <- final_data2[train_index, ]
test_data  <- final_data2[-train_index, ]

# Check distributions (should be similar)
prop.table(table(train_data$hdi_category))
prop.table(table(test_data$hdi_category))

# Baseline model: Multinomial Logistic Regression using caret
ctrl <- trainControl(method = "cv", number = 5)

model_logit <- train(
  hdi_category ~ log_gdp_per_capita + log_population + log_gdp,
  data = train_data,
  method = "multinom",
  trControl = ctrl
)

# Predictions
pred_logit <- predict(model_logit, newdata = test_data)

# Evaluation
confusionMatrix(pred_logit, test_data$hdi_category)

# Show model details
model_logit



# STEP 13A: Install & Load Random Forest Packages

packages2 <- c("randomForest")
new_packages2 <- packages2[!(packages2 %in% installed.packages()[,"Package"])]
if(length(new_packages2)) install.packages(new_packages2)

library(randomForest)

cat("✅ RandomForest package loaded!\n")


# STEP 13B: Train Random Forest Model


set.seed(123)

ctrl_rf <- trainControl(method = "cv", number = 5)

model_rf <- train(
  hdi_category ~ log_gdp_per_capita + log_population + log_gdp,
  data = train_data,
  method = "rf",
  trControl = ctrl_rf,
  importance = TRUE
)

# Predictions
pred_rf <- predict(model_rf, newdata = test_data)

# Evaluation
confusionMatrix(pred_rf, test_data$hdi_category)

# Print model summary
model_rf


# STEP 13C: Feature Importance


varImp_rf <- varImp(model_rf)
print(varImp_rf)

plot(varImp_rf, main = "Random Forest Feature Importance")


# STEP 13D: Compare Logistic Regression vs Random Forest

logit_results <- confusionMatrix(pred_logit, test_data$hdi_category)
rf_results <- confusionMatrix(pred_rf, test_data$hdi_category)

comparison <- data.frame(
  Model = c("Multinomial Logistic Regression", "Random Forest"),
  Accuracy = c(logit_results$overall["Accuracy"], rf_results$overall["Accuracy"]),
  Kappa = c(logit_results$overall["Kappa"], rf_results$overall["Kappa"])
)

print(comparison)




# STEP 14: Export Dataset + Results + Plots


# 1) Save final cleaned dataset
write.csv(final_data2, "final_wikipedia_dataset.csv", row.names = FALSE)

# 2) Save model comparison results
write.csv(comparison, "model_comparison_results.csv", row.names = FALSE)

cat("✅ CSV files saved: final_wikipedia_dataset.csv and model_comparison_results.csv\n")


# 3) Save EDA Plot 1: GDP per Capita vs HDI
png("plot_gdp_per_capita_vs_hdi.png", width = 900, height = 600)
ggplot(final_data2, aes(x = gdp_per_capita, y = hdi)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  labs(
    title = "GDP per Capita vs HDI",
    x = "GDP per Capita (log scale)",
    y = "HDI"
  ) +
  theme_minimal()
dev.off()

# 4) Save EDA Plot 2: GDP per Capita by HDI Category
png("plot_gdp_per_capita_by_hdi_category.png", width = 900, height = 600)
ggplot(final_data2, aes(x = hdi_category, y = gdp_per_capita)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(
    title = "GDP per Capita by HDI Category",
    x = "HDI Category",
    y = "GDP per Capita (log scale)"
  ) +
  theme_minimal()
dev.off()

cat("✅ Plots saved as PNG files in your working directory\n")


# 5) Save Feature Importance Plot
png("plot_rf_feature_importance.png", width = 900, height = 600)
plot(varImp_rf, main = "Random Forest Feature Importance")
dev.off()

cat("✅ Feature importance plot saved: plot_rf_feature_importance.png\n")
