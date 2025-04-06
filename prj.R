# ----------------------------
# 0. Package Installation & Setup
# ----------------------------
# Install required packages (only needed once)
install.packages(c("ggplot2", "dplyr", "corrplot", "caret", "lattice"))

# Load all required libraries
library(ggplot2)    # For data visualization
library(dplyr)      # For data manipulation
library(corrplot)   # For correlation visualization
library(caret)      # For machine learning utilities
library(lattice)    # For statistical graphics

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

#Showing distribution of all categorical variables
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
# Create backup of original data
data_original <- flight_data

# Remove unnecessary columns and convert types
flight_data <- flight_data %>%
  select(-X) %>%                       # Remove index column if exists
  mutate_if(is.character, as.factor)   # Convert character columns to factors

# Split data into training and testing sets
train_index <- createDataPartition(flight_data$price, p = 0.7, list = FALSE)
train_data <- flight_data[train_index, ]
test_data <- flight_data[-train_index, ]

# ----------------------------
# 4. Exploratory Data Analysis (EDA)
# ----------------------------
## 4.1 Price Distribution Analysis
plot_price_distribution <- function(data) {
  # Histogram
  hist_plot <- ggplot(data, aes(x = price)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black") +
    theme_minimal() +
    labs(title = "Flight Price Distribution", 
         x = "Ticket Price (USD)", 
         y = "Frequency")
  
  # Boxplot
  box_plot <- ggplot(data, aes(y = price)) +
    geom_boxplot(fill = "orange") +
    theme_minimal() +
    labs(title = "Price Distribution Overview", 
         y = "Ticket Price (USD)")
  
  # Display plots
  print(hist_plot)
  print(box_plot)
  
  # Skewness (check if 'moments' is already installed before installing again)
  if (!require(moments)) install.packages("moments", dependencies = TRUE)
  library(moments)
  
  cat("Skewness of price distribution: ", skewness(data$price), "\n")
}

plot_price_distribution(flight_data)

#Here, skewness > 1, that's why it should be interesting to log-transform for linear regression 
#Price can't be equal to 0, so we shouldn't have an issue

## 4.2 Correlation Analysis
plot_correlation_matrix <- function(data) {
  # Select only numeric columns
  numeric_cols <- sapply(data, is.numeric)
  cor_matrix <- cor(data[, numeric_cols], use = "complete.obs")
  
  # Create correlation plot
  corrplot(cor_matrix, 
           method = "circle", 
           type = "upper", 
           tl.col = "black", 
           tl.srt = 45,
           title = "Correlation Matrix of Numerical Variables", 
           mar = c(0, 0, 1, 0))
}

plot_correlation_matrix(flight_data)

#Plot Box Plot regarding departure_time, arrival_time and class
plot_boxplot_arrival_time <- function(data) {
  ggplot(data, aes(x = arrival_time, y = price, fill = arrival_time)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Price by Arrival Time", x = "Arrival Time", y = "Price") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_boxplot_class <- function(data) {
  ggplot(data, aes(x = class, y = price, fill = class)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Price by Class", x = "Class", y = "Price")
}

plot_boxplot_departure_time <- function(data) {
  ggplot(data, aes(x = departure_time, y = price, fill = departure_time)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Price by Departure Time", x = "Departure Time", y = "Price") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_boxplot_departure_time(flight_data)
plot_boxplot_class(flight_data)
plot_boxplot_arrival_time(flight_data)

#Null Hypothesis Testing (Class)
#Let µ0: Business Class
#Let µ1: Economy Class
#Null Hypothesis: H0 => µ0 = µ1

t.test(price ~ class, data = flight_data)

#p-value --> < 2.2e-16, so H0 can be rejected 

#Comparing Night-Flights and Day-Flights | Comparing Stops and Non-Stops
#Null Hypothesis Testing
#Let µ0: Day-Flights
#Let µ1 : Night-flights

flight_data_night_day_vers <- flight_data
# Add two binary variables (Day/Night) and (Non-stop/stop)
flight_data_night_day_vers$time_of_day <- ifelse(flight_data$departure_time %in% c("Morning", "Afternoon", "Evening"),
                                                 "Day", "Night")
flight_data_night_day_vers$time_of_day <- factor(flight_data_night_day_vers$time_of_day, levels = c("Day", "Night"))
levels(flight_data_night_day_vers$time_of_day) #Day / "Night"
t.test(price ~ time_of_day, data = flight_data_night_day_vers, alternative="greater")
#p_value = 0.002357, so it seems that day flights are cheaper than night flights

with_stop_data <- flight_data %>%
  filter(stops %in% c("one", "two_or_more"))

long_flights <- with_stop_data %>%
  filter(duration >= 32, duration <= 40)

table(long_flights$stops)

t.test(price ~ stops, data = long_flights, alternative="less") #Not enough interesting
#Maybe, just quote it as a try but not answer due to not statically significant

summary(lm(price ~ stops + duration, data = with_stop_data))

#Holding flight duration constant, flights with multiple stops are statistically 
#significantly cheaper than those with a single stop (−$10,743 on average, p < 0.001), 
#indicating that this difference is highly unlikely to be due to random chance.
#However, needs to say that our R-squared is very low, so we can't explain only
#by this model, but show the impact of a stop or not (at same duration, one stop is 
#cheaper than two or more)

#Comparing between Airplane airlines (with same duration)

two_airlines <- flight_data %>%
  filter(airline %in% c("Air_India", "Vistara"))

airlines_similar_duration <- two_airlines %>%
  filter(duration >= 2, duration <= 3)

table(airlines_similar_duration$airline)

airlines_similar_duration$airline <- factor(airlines_similar_duration$airline, levels = c("Vistara", "Air_India"))

t.test(price ~ airline, data = airlines_similar_duration, alternative = "greater")
#p-value <- 0.001841

#For comparable durations (between 2 and 3 hours), flights operated by Vistara are 
#statistically more expensive than those operated by Air India, with an average 
#price difference of €3,631. This difference is statistically significant (p = 0.0018).

# ----------------------------
# 5. Baseline Model Development
# ----------------------------
## 5.1 Data Preparation for Modeling
prepare_flight_data <- function(train, test) {
  # Handle factor levels in training data
  train$flight <- factor(as.character(train$flight))
  
  # Add "Other" level if not present
  if (!("Other" %in% levels(train$flight))) {
    levels(train$flight) <- c(levels(train$flight), "Other")
  }
  
  # Prepare test data with matching factor levels
  test$flight <- as.character(test$flight)
  allowed_levels <- levels(train$flight)
  test$flight[!(test$flight %in% allowed_levels)] <- "Other"
  test$flight <- factor(test$flight, levels = allowed_levels)
  
  return(list(train = train, test = test))
}

prepared_data <- prepare_flight_data(train_data, test_data)
train_data <- prepared_data$train
test_data <- prepared_data$test

## 5.2 Linear Regression Model
# Train model
lm_model <- lm(price ~ ., data = train_data)
summary(lm_model)

# Make predictions and evaluate
predictions <- predict(lm_model, newdata = test_data)
mse <- mean((test_data$price - predictions)^2)

# Print evaluation metrics
cat("Baseline Model Performance:\n")
cat("--------------------------\n")
cat("Mean Squared Error:", mse, "\n")
cat("Root Mean Squared Error:", sqrt(mse), "\n")