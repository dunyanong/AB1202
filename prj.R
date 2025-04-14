
data_file <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)
if ("X" %in% names(data_file)) {
  data_file <- data_file %>% select(-X)
}


library(dplyr)
library(ggplot2)
library(corrplot)
library(moments)
library(ggpubr)    


cat("nidm：", dim(data_file), "\n\n")
str(data_file)

print(sapply(data_file, function(x) sum(is.na(x))))


print(summary(select_if(data_file, is.numeric)))


if ("duration" %in% names(data_file)) {
  data_file$duration <- as.numeric(gsub(",", ".", data_file$duration))
}
for (col in c("airline","flight","source_city","destination_city",
              "departure_time","arrival_time","class")) {
  if (col %in% names(data_file)) {
    data_file[[col]] <- as.factor(data_file[[col]])
  }
}


num_cols <- names(select_if(data_file, is.numeric))
cat_cols <- names(select_if(data_file, is.factor))

for (col in num_cols) {
  print(
    ggplot(data_file, aes_string(col)) +
      geom_histogram(bins = 30, alpha = 0.6) +
      labs(title = paste("Distribution of", col), x = col, y = "Count")
  )
}


for (col in cat_cols) {
  print(
    ggplot(data_file, aes_string(col)) +
      geom_bar(alpha = 0.6) +
      labs(title = paste("Counts of", col), x = col, y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

# 7. corr and skewness
if (length(num_cols) >= 2) {
  mat <- cor(select(data_file, all_of(num_cols)), use = "complete.obs")
  corrplot(mat, method = "shade", tl.cex = 0.8, title = "Numeric Correlation Matrix")
}
for (col in num_cols) {
  cat(sprintf("skewness) of %s: %.3f\n",
              col, skewness(data_file[[col]], na.rm = TRUE)))
}

# 8. bivariate
# 8.1 ：sctter + LOESS
for (i in seq_along(num_cols)[-length(num_cols)]) {
  for (j in (i+1):length(num_cols)) {
    xcol <- num_cols[i]
    ycol <- num_cols[j]
    
    logx <- paste0("log_", xcol)
    logy <- paste0("log_", ycol)
  

    data_file[[logx]] <- log(data_file[[xcol]] + 1e-6)
    data_file[[logy]] <- log(data_file[[ycol]] + 1e-6)
    
    print(
      ggplot(data_file, aes_string(x = logx, y = logy)) +
        geom_point(alpha = 0.3) +
        geom_smooth(method = "lm", se = FALSE) +     
        labs(
          title = paste("log(", ycol, ") vs log(", xcol, ")", sep=""),
          x = paste("log(", xcol, ")"),
          y = paste("log(", ycol, ")")
        ) +
        theme_minimal()
    )
    
    
    data_file[[logx]] <- NULL
    data_file[[logy]] <- NULL
  }
}

# 8.2 
for (ncol in num_cols) {
  for (ccol in cat_cols) {
    print(
      ggplot(data_file, aes_string(x = ccol, y = ncol)) +
        geom_boxplot(outlier.shape = NA, alpha = 0.4) +
        geom_jitter(width = 0.2, alpha = 0.3, size = 0.8) +
        labs(title = paste(ncol, "by", ccol), x = ccol, y = ncol) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  }
}

# 9.  source_city & destination_city ave price 95% CI
ci_src <- data_file %>%
  group_by(source_city) %>%
  summarise(n = n(),
            mean_price = mean(price, na.rm = TRUE),
            sd_price = sd(price, na.rm = TRUE)) %>%
  filter(n >= 30) %>%
  mutate(se = sd_price / sqrt(n),
         ci_low  = mean_price - qt(0.975, df = n-1) * se,
         ci_high = mean_price + qt(0.975, df = n-1) * se)

ci_dest <- data_file %>%
  group_by(destination_city) %>%
  summarise(n = n(),
            mean_price = mean(price, na.rm = TRUE),
            sd_price = sd(price, na.rm = TRUE)) %>%
  filter(n >= 30) %>%
  mutate(se = sd_price / sqrt(n),
         ci_low  = mean_price - qt(0.975, df = n-1) * se,
         ci_high = mean_price + qt(0.975, df = n-1) * se)

# confidence nterval
print(
  ggplot(ci_src, aes(x = reorder(source_city, mean_price), y = mean_price)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
    labs(title = "Mean Price with 95% CI by Source City",
         x = "Source City", y = "Mean Price") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)

print(
  ggplot(ci_dest, aes(x = reorder(destination_city, mean_price), y = mean_price)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
    labs(title = "Mean Price with 95% CI by Destination City",
         x = "Destination City", y = "Mean Price") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)


# 10.1 Economy vs Business（two tail t）
t_econ_bus <- t.test(price ~ class,
                     data = data_file %>% filter(class %in% c("Economy","Business")),
                     conf.level = 0.95)
print(t_econ_bus)

# boxplot + p‑value 
print(
  ggplot(data_file %>% filter(class %in% c("Economy","Business")),
         aes(x = class, y = price)) +
    geom_boxplot() +
    stat_compare_means(method = "t.test", label = "p.format") +
    labs(title = "Price by Class with t‑test p‑value")
)

# 10.2  ANOVA：Airline（仅 Economy）
anova_air <- aov(price ~ airline,
                 data = data_file %>% filter(class == "Economy"))
print(summary(anova_air))


# 10.3 Departure Time ANOVA
anova_dep <- aov(price ~ departure_time, data = data_file)
print(summary(anova_dep))


# 11 popular routes
if (all(c("source_city","destination_city","price") %in% names(data_file))) {
  top_routes <- data_file %>%
    group_by(source_city, destination_city) %>%
    summarise(n = n(), avg_price = mean(price, na.rm=TRUE)) %>%
    arrange(desc(n)) %>%
    slice(1:5) %>%
    mutate(route = paste(source_city, "→", destination_city))
  
  print(top_routes)
  
  print(
    data_file %>%
      mutate(route = paste(source_city, "→", destination_city)) %>%
      filter(route %in% top_routes$route) %>%
      ggplot(aes(route, price, fill = route)) +
      geom_boxplot() +
      labs(title = "Top 3 Routes: Price Comparison") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}



set.seed(123)
n <- nrow(data_file)
train_idx <- sample(seq_len(n), size = floor(0.7 * n))
train_data <- droplevels(data_file[train_idx, ])


factor_cols <- names(train_data)[sapply(train_data, is.factor)]
levels_count <- sapply(train_data[factor_cols], nlevels)
print(levels_count)


single_level <- names(levels_count[levels_count < 2])


remove 1 level IV
train_data2 <- train_data %>% select(-all_of(single_level))
test_data2  <- test_data  %>% select(-all_of(single_level))

lm_model <- lm(log(price) ~ airline 
               + source_city  
               + departure_time + arrival_time 
               + class + duration + days_left,
               data = train_data2)

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
