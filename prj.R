# ----------------------------
# 0. Package Installation & Setup
# ----------------------------
# Install required packages (only needed once)
install.packages(c("ggplot2", "dplyr", "corrplot", "caret", "lattice", "moments"))

# Load all required libraries
library(ggplot2)    # For data visualization
library(dplyr)      # For data manipulation
library(corrplot)   # For correlation visualization
library(caret)      # For machine learning utilities
library(lattice)    # For statistical graphics
library(moments)    # For skewness calculation

# Set random seed for reproducibility
set.seed(123)

# ----------------------------
# 1. Data Loading & Initial Inspection
# ----------------------------
# Load the dataset
flight_data <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)
flight_data <- flight_data[sample(nrow(flight_data), 10000), ]
flight_data <- flight_data[, !(names(flight_data) %in% "flight")]

# Initial data inspection
View(flight_data)
str(flight_data)
summary(flight_data)
colSums(is.na(flight_data))

# ----------------------------
# 2. Categorical Variable Analysis
# ----------------------------
# Analyze frequency of categorical variables
analyze_categorical_variables <- function(data) {
  # Identify character columns
  string_cols <- names(data)[sapply(data, is.character)]
  
  # Create frequency tables
  freq_tables <- lapply(data[string_cols], table)
  
  # Convert to data frames and combine
  df_list <- lapply(freq_tables, function(tbl) as.data.frame(tbl))
  
  combined_df <- do.call(rbind, lapply(names(df_list), function(name) {
    df <- df_list[[name]]
    df$variable <- name
    names(df)[1:2] <- c("value", "frequency")
    df[, c("variable", "value", "frequency")]
  }))
  
  # Remove flight column if present
  combined_df <- combined_df[combined_df$variable != "flight", ]
  return(combined_df)
}

categorical_summary <- analyze_categorical_variables(flight_data)
print(categorical_summary)

# Visualizing distribution of all categorical variables
vars_to_plot <- c("class", "departure_time", "stops", "source_city", "destination_city")
for (v in vars_to_plot) {
  df_subset <- filter(categorical_summary, variable == v)
  p <- ggplot(df_subset, aes(x = reorder(value, -frequency), y = frequency, fill = value)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Distribution of", v),
         x = v,
         y = "Frequency") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = FALSE)
  print(p)
}

# ----------------------------
# 3. Data Preprocessing
# ----------------------------
# Backup original data
data_original <- flight_data

# Remove unnecessary columns and convert character columns to factors
flight_data <- flight_data %>%
  select(-X) %>%                       # Remove index column if exists
  mutate_if(is.character, as.factor)   # Convert character columns to factors

# Log transform the price variable (due to skewness)
flight_data$log_price <- log(flight_data$price + 1)

# Split data into training and testing sets
train_index <- createDataPartition(flight_data$log_price, p = 0.7, list = FALSE)
train_data <- flight_data[train_index, ]
test_data <- flight_data[-train_index, ]

# ----------------------------
# 4. Exploratory Data Analysis (EDA)
# ----------------------------
## 4.1 Price Distribution Analysis
plot_price_distribution <- function(data) {
  # Histogram
  hist_plot <- ggplot(data, aes(x = log_price)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black") +
    theme_minimal() +
    labs(title = "Flight Price Distribution (Log Transformed)", 
         x = "Log Ticket Price (USD)", 
         y = "Frequency")
  
  # Boxplot
  box_plot <- ggplot(data, aes(y = log_price)) +
    geom_boxplot(fill = "orange") +
    theme_minimal() +
    labs(title = "Price Distribution Overview (Log Transformed)", 
         y = "Log Ticket Price (USD)")
  
  # Display plots
  print(hist_plot)
  print(box_plot)
  
  # Skewness (check if 'moments' is already installed before installing again)
  cat("Skewness of price distribution: ", skewness(data$log_price), "\n")
}

plot_price_distribution(flight_data)

## 4.2 Correlation Analysis
plot_correlation_matrix <- function(data) {
  # Select only numeric columns
  numeric_cols <- sapply(data, is.numeric)
  cor_matrix <- cor(data[, numeric_cols], use = "complete.obs")
  
  # Create correlation plot
  corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45,
           title = "Correlation Matrix of Numerical Variables", mar = c(0, 0, 1, 0))
  
  cat("Pearson Correlation Matrix:\n")
  print(cor_matrix)
}

plot_correlation_matrix(flight_data)

# ----------------------------
# 5. Baseline Model Development
# ----------------------------
## 5.1 Linear Regression Model
# Train model
lm_model <- lm(log_price ~ ., data = train_data)
summary(lm_model)

# Make predictions and evaluate
predictions <- predict(lm_model, newdata = test_data)
mse <- mean((test_data$log_price - predictions)^2)

# Print evaluation metrics
cat("Baseline Model Performance:\n")
cat("--------------------------\n")
cat("Mean Squared Error:", mse, "\n")
cat("Root Mean Squared Error:", sqrt(mse), "\n")
cat("R-squared:", summary(lm_model)$r.squared, "\n")
cat("Adjusted R-squared:", summary(lm_model)$adj.r.squared, "\n")

