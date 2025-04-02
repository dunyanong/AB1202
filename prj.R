install.packages("ggplot2")
install.packages("dplyr")
install.packages("corrplot") #this n above for visualize
install.packages("caret")

# 1 load the file and look the data
data <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)
View(data)
str(data)
summary(data)
colSums(is.na(data))

# 2 how many type under a column, the amount of each type
string_cols <- names(data)[sapply(data, is.character)]
result <- lapply(data[string_cols], table)
df_list <- lapply(result, function(tbl) as.data.frame(tbl))

df_combined <- do.call(rbind, lapply(names(df_list), function(name) {
  df <- df_list[[name]]
  df$variable <- name

  names(df)[1:2] <- c("value", "frequency")
  df <- df[, c("variable", "value", "frequency")]
  df
}))

df_combined
df_combined <- df_combined[df_combined$variable != "flight", ]
df_combined

# 3 Data preprocess
library(caret)
set.seed(123)

data_ori <- data
if("X" %in% names(data)) {
  data <- data %>% select(-X)
}
data <- data %>% mutate_if(is.character, as.factor)
train_index <- createDataPartition(data$price, p = 0.7, list = FALSE)
ln_train_data <- data[train_index, ]
ln_test_data <- data[-train_index, ]

# 4 EDA and visualization

library(ggplot2)
library(dplyr)
library(corrplot)

  #price distribution
price_distribution_histogram <- ggplot(data, aes(x = price)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "price distribution", x = "ticket price", y = "frequency")
price_distribution_boxplot <- ggplot(data, aes(y = price)) +
  geom_boxplot(fill = "orange") +
  theme_minimal() +
  labs(title = "price distribution", y = "ticket price")
  print(price_distribution_boxplot)
  #corr and heatmap
numeric_cols <- sapply(data, is.numeric)
cor_data <- cor(data[, numeric_cols], use = "complete.obs")
corrplot(cor_data, method = "circle", type = "upper", tl.col = "black", tl.srt = 45,
         title = "corr of int type variable", mar = c(0,0,1,0))
 # if want further, normalize column data in str to plot corr

# 5 baseline model

ln_train_data$flight <- factor(as.character(ln_train_data$flight))
if(!("Other" %in% levels(ln_train_data$flight))){
  levels(ln_train_data$flight) <- c(levels(ln_train_data$flight), "Other")
}
ln_test_data$flight <- as.character(ln_test_data$flight)
allowed_levels <- levels(ln_train_data$flight)
ln_test_data$flight[!(ln_test_data$flight %in% allowed_levels)] <- "Other"
ln_test_data$flight <- factor(ln_test_data$flight, levels = allowed_levels)

lm_model <- lm(price ~ ., data = ln_train_data)
summary(lm_model)

predictions_lm <- predict(lm_model, newdata = ln_test_data)
mse_lm <- mean((ln_test_data$price - predictions_lm)^2)
cat("MSE:", mse_lm, "\n")

  # Signif. codes:
# 这行说明了统计显著性的符号表示方法。
# 
# ‘***’ 表示 p 值 < 0.001
# 
# ‘**’ 表示 0.001 ≤ p 值 < 0.01
# 
# ‘*’ 表示 0.01 ≤ p 值 < 0.05
# 
# ‘.’ 表示 0.05 ≤ p 值 < 0.1
# 
# ‘ ’（空格）表示 p 值 ≥ 0.1
# 当模型中各个系数后面显示这些符号时，可以直观地看出哪些变量在统计上是显著的。
# 
# Residual standard error: 6191 on 208526 degrees of freedom
# 
# Residual standard error（残差标准误差）： 表示模型预测值与实际值之间的平均偏差，大约为 6191。
# 
# Degrees of freedom（自由度）： 这里是 208526，自由度通常等于样本数减去估计的参数个数（包括截距）。
# 
# Multiple R-squared: 0.926, Adjusted R-squared: 0.9255
# 
# Multiple R-squared（多重决定系数）： 0.926 表示模型可以解释大约 92.6% 的目标变量（price）的变异性。
# 
# Adjusted R-squared（调整后的决定系数）： 考虑了模型中变量个数，0.9255 表示在加入解释变量后仍然有大约 92.55% 的变异性被解释。调整后的 R²更适合用于比较不同数量自变量的模型。
# 
# F-statistic: 1650 on 1582 and 208526 DF, p-value: < 2.2e-16
# 
# F-statistic（F统计量）： 1650 是检验整个模型（即所有自变量）是否在统计上显著的一个指标。
# 
# DF（自由度）： 模型中自变量的个数是 1582（分子自由度），而误差自由度是 208526（分母自由度）。
# 
# p-value: 小于 2.2e-16，说明整个模型在统计上非常显著，即至少有一个自变量对目标变量有显著影响。
# 
# 总结来说，这段输出告诉我们：模型拟合得比较好（R² 很高），残差的平均误差大约是 6191，整个模型显著性检验的结果非常显著（p 值极小），同时提供了各变量显著性水平的符号说明
