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

data_file <- read.csv(file.choose(), sep = ";")
library(dplyr)
library(ggplot2)
library(moments)

#Clean duration column into numeric values
data_file$duration <- as.numeric(gsub(",", ".", data_file$duration))

#Analyse for the Mumbai-Delhi flight
#------------------------------------------

filtered_data_mumbai_delhi <- data_file %>%
  filter(source_city == "Mumbai", destination_city == "Delhi")

#Give a first expectation for the price
#So we seek for µ, which µ is the true price mean for this flight

#For a confidence-level of 0.9
t.test(filtered_data_mumbai_delhi$price, conf.level=0.9)
#90 percent confidence interval:
#25136.46 26645.50

#For a confidence-level of 0.95
t.test(filtered_data_mumbai_delhi$price, conf.level=0.95)
#95 percent confidence interval:
#24991.77 26790.20

#For a confidence-level of 0.99
t.test(filtered_data_mumbai_delhi$price, conf.level=0.99)
#99 percent confidence interval:
#24708.72 27073.24

#Simple Analysis (without multivariable)
#Please precise that our first try was to see if there was a signficiant factor 
#that had a huge impact on price, without a multi-factor analysis
#To see if the company should be focused on one specific factor to estimate its
#price

#Correlation between price and duration
cor(filtered_data_mumbai_delhi$price, filtered_data_mumbai_delhi$duration, use = "complete.obs")
#0.01707316 (Very weak)

#Correlation graphical interpretation
ggplot(filtered_data_mumbai_delhi, aes(x = duration, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Duration vs Price for Mumbai - Delhi",
       x = "Duration (hours)",
       y = "Price")

#Allows to show that the correlation between them is very hard to find (a lot of errors)

#Check if skweness of price is > 1
skewness(filtered_data_mumbai_delhi$price)
#[1] 0.1565336

#Check what the time of departure/arrival say about the price
# For departure_time (Early Morning base group)
filtered_data_mumbai_delhi$departure_time <- factor(filtered_data_mumbai_delhi$departure_time)

# For arrival_time (Early Morning base group)
filtered_data_mumbai_delhi$arrival_time <- factor(filtered_data_mumbai_delhi$arrival_time)

#No need to use log because we checked skewness < 1
model <- lm(price ~ arrival_time + departure_time, data = filtered_data_mumbai_delhi)
summary(model)

levels(filtered_data_mumbai_delhi$departure_time)
levels(filtered_data_mumbai_delhi$arrival_time)

#We found these results
'Coefficients:
                            Estimate Std. Error t value Pr(>|t|)    
(Intercept)                    20372       2217   9.188  < 2e-16 ***
arrival_timeEarly_Morning      -8154       3415  -2.388  0.01707 *  
arrival_timeEvening             3948       1699   2.324  0.02026 *  
arrival_timeLate_Night          1440       3645   0.395  0.69276    
arrival_timeMorning            -4083       1793  -2.278  0.02287 *  
arrival_timeNight               3054       1841   1.659  0.09740 .  
departure_timeEarly_Morning    10179       3180   3.201  0.00139 ** 
departure_timeEvening           1745       1813   0.962  0.33605    
departure_timeMorning           7615       2401   3.172  0.00154 ** 
departure_timeNight             4625       1907   2.425  0.01541 *  '

#I think we should exclude Arrival Late Night and Night (as they arent't 
#statistcally signfiicant), as well as departure Evening

#But we can see that under 5% of significance, arrival_time Early_Morning and 
#departure_time at Night seems to be the best duo 

#We count how many there are of data
sum(filtered_data_mumbai_delhi$departure_time == "Night" & filtered_data_mumbai_delhi$arrival_time == "Early Morning")
#0 which is equal to 0, so wee need to improve our model by using a same category
#Departure_time/Arrival_time

#Departure_time and arrival_time combined:
filtered_data_mumbai_delhi$time_duo <- paste(filtered_data_mumbai_delhi$departure_time, filtered_data_mumbai_delhi$arrival_time, sep = " / ")
filtered_data_mumbai_delhi$time_duo <- as.factor(filtered_data_mumbai_delhi$time_duo)

model1 <- lm(price ~ time_duo, data = filtered_data_mumbai_delhi)
summary(model1)

'
                                     Pr(>|t|)   
(Intercept)                            0.00361 **
time_duoAfternoon / Evening            0.10584   
time_duoEarly_Morning / Early_Morning  0.08895 . 
time_duoEarly_Morning / Late_Night     0.23443   
time_duoEarly_Morning / Morning        0.13627   
time_duoEvening / Afternoon            0.09303 . 
time_duoEvening / Evening              0.10560   
time_duoEvening / Night                0.13835   
time_duoMorning / Afternoon            0.14402   
time_duoMorning / Evening              0.33213   
time_duoMorning / Morning              0.10302   
time_duoNight / Evening                0.20524   
time_duoNight / Night                  0.12241 
'
#Due to p_value, impossible to conclude

#Hypothesis Testing with Business and Economy
t.test(filtered_data_mumbai_delhi$price[filtered_data_mumbai_delhi$class=="Economy"], filtered_data_mumbai_delhi$price[filtered_data_mumbai_delhi$class=="Business"], alternative="less")
#p_value <- 2.2e-16 so we know that price in Economy < price in Business


#Hypothesis Testing with Air_India and Vistara
t.test(filtered_data_mumbai_delhi$price[filtered_data_mumbai_delhi$airline=="Air_India"&filtered_data_mumbai_delhi$class=="Economy"], filtered_data_mumbai_delhi$price[filtered_data_mumbai_delhi$airline=="Vistara"&filtered_data_mumbai_delhi$class=="Economy"], alternative="less")

#p-value = 0.0005188
#so price of Air India < price of Vistara with high statiscally significance, for economy

#Check what the day-left say about the price
cor(filtered_data_mumbai_delhi$price, filtered_data_mumbai_delhi$days_left, use = "complete.obs")
#-0.100841 --> No  very high correlation but not bad 

#Construct a linear model
model2 <- lm(price~class+departure_time+arrival_time+airline+duration+days_left, data=filtered_data_mumbai_delhi)
#Adjusted R^2 : 0.9095

#Analyse of factors:
#ELBE: 
#
#classEconomy                -35353.38     
#departure_timeEarly_Morning   1705.85     
#departure_timeEvening        -1346.92    
#departure_timeMorning          702.91    
#departure_timeNight           1630.15    
#arrival_timeEarly_Morning    -3133.10    
#arrival_timeEvening           1635.40     
#arrival_timeLate_Night        3017.56    
#arrival_timeMorning           -194.47     
#arrival_timeNight             3504.49     
#airlineVistara                1200.60     
#days_left                     -169.25

filtered_time_duo <- filtered_data_mumbai_delhi %>%
  group_by(time_duo) %>%
  filter(n() > 70) %>%
  ungroup()

filtered_time_duo$time_duo <- droplevels(filtered_time_duo$time_duo)
table(filtered_time_duo$time_duo)

model3 <- lm(price~class+time_duo+duration+days_left+airline, data=filtered_time_duo)
levels(filtered_time_duo$time_duo) #To see that is in comparison with "Afternoon / Evening"

#With pretty good p_values, apart from time_duoEarly_Morning / Morning & time_duoMorning / Afternoon (which are not rejected under 10% signficance)
'
Coefficients:
                                 Estimate Std. Error
(Intercept)                      84239.16    4094.86
classEconomy                    -35933.55     294.67
time_duoEarly_Morning / Morning    -24.14     683.62
time_duoEvening / Afternoon     -12637.75    1165.25
time_duoEvening / Evening        -6051.66     714.55
time_duoEvening / Night           2554.65     599.00
time_duoMorning / Afternoon       -155.19     630.90
time_duoMorning / Morning        -5769.17     758.01
time_duoNight / Evening          -3746.98    1017.49
time_duoNight / Night             -754.90     724.70
duration                         -1359.40     148.11
days_left                         -175.47      10.19
airlineVistara                     822.24     330.29
'

#Regarding this graphics, it should be better to take Evening/Afternoon
#We check if it is highly available or not

unique(filtered_data_mumbai_delhi[filtered_data_mumbai_delhi$time_duo="Evening / Afternoon",]$days_left)
'
[1]  1  2  3  4  5  6  7 10 11 12 13 14 16 17 18 19
[17] 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35
[33] 36 37 38 39 40 41 42 44 45 46 47 48 49  8  9 15
[49] 43
'
#So it is highly available 

#Summary
"""
Our company should above all focus on the class of its work team to go down the price, 
by choosing an Economy class. 

Then, it should take some tries to choose a flight from Evening to Afternoon, or at least from Evening to Evening or Morning to Morning. 

Compared with these two elements, duration, days_left or even airline don't have a huge impact, though the company could fine tune its choice 
by selectinga a flight from Air_India, with the least duration possible and the earliest. 
"""


