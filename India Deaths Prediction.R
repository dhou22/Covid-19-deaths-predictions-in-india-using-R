# COVID-19 Deaths Analysis in India




################## 1. LIBRARY IMPORTS ##################
library(tidyverse)
library(skimr)
library(DataExplorer)
library(corrplot)
library(moments)
library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)
library(lubridate)
library(rmarkdown)
library(knitr)
library(DT)






################## 2. DATA UNDERSTANDING  ##################

# Comprehensive data overview function
data_overview <- function(data) {
  cat("\n=== Basic Data Summary ===\n")
  
  # Dimensions and structure
  cat("Dimensions (Rows, Columns): ", dim(data), "\n")
  cat("\nStructure:\n")
  str(data)
  
  # Data types and missing values
  cat("\nData Types:\n")
  print(sapply(data, class))
  cat("\nMissing Values:\n")
  print(colSums(is.na(data)))
  
  # Basic statistics for numeric columns
  cat("\nNumerical Summary:\n")
  print(summary(data))
}

# Enhanced numerical analysis function
numerical_analysis <- function(data) {
  num_cols <- sapply(data, is.numeric)
  num_data <- data[, num_cols]
  
  cat("\n=== Numerical Analysis ===\n")
  
  stats_df <- data.frame(
    Mean = sapply(num_data, mean, na.rm = TRUE),
    Median = sapply(num_data, median, na.rm = TRUE),
    SD = sapply(num_data, sd, na.rm = TRUE),
    Skewness = sapply(num_data, skewness, na.rm = TRUE),
    Kurtosis = sapply(num_data, kurtosis, na.rm = TRUE),
    Q1 = sapply(num_data, quantile, probs = 0.25, na.rm = TRUE),
    Q3 = sapply(num_data, quantile, probs = 0.75, na.rm = TRUE)
  )
  
  print(stats_df)
  
  # Correlation matrix visualization
  cor_matrix <- cor(num_data, use = "complete.obs")
  corrplot(cor_matrix,
           method = "color",
           type = "upper",
           addCoef.col = "black",
           tl.col = "black",
           tl.srt = 45)
}

# Enhanced visualization function
create_visualizations <- function(data, title = "Data Visualization") {
  # Correlation heatmap
  cor_matrix <- cor(data[, sapply(data, is.numeric)], use = "complete.obs")
  melted_cor_matrix <- melt(cor_matrix)
  
  heatmap <- ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
    theme_minimal() +
    labs(title = "Correlation Matrix Heatmap", x = "", y = "") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Boxplots for outlier detection
  numeric_data <- data[, sapply(data, is.numeric)]
  melted_data <- reshape2::melt(numeric_data, variable.name = "Feature", value.name = "Value")
  
  boxplots <- ggplot(melted_data, aes(x = Feature, y = Value)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
    theme_minimal() +
    labs(title = "Outlier Detection Boxplots", x = "Features", y = "Values") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Arrange plots
  grid.arrange(heatmap, boxplots, ncol = 1)
}







##################     3. DATA PREPARATION      ##################

# Handle missing data
handle_missing_data <- function(dataset) {
  for (col_name in colnames(dataset)) {
    if (is.numeric(dataset[[col_name]])) {
      dataset[[col_name]][is.na(dataset[[col_name]])] <- median(dataset[[col_name]], na.rm = TRUE)
    } else {
      mode_value <- names(sort(table(dataset[[col_name]]), decreasing = TRUE))[1]
      dataset[[col_name]][is.na(dataset[[col_name]])] <- mode_value
    }
  }
  return(dataset)
}

# Handle outliers using IQR method
handle_outliers <- function(dataset) {
  for (col_name in colnames(dataset)) {
    if (is.numeric(dataset[[col_name]])) {
      Q1 <- quantile(dataset[[col_name]], 0.25, na.rm = TRUE)
      Q3 <- quantile(dataset[[col_name]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      dataset[[col_name]][dataset[[col_name]] < lower_bound | 
                            dataset[[col_name]] > upper_bound] <- 
        median(dataset[[col_name]], na.rm = TRUE)
    }
  }
  return(dataset)
}

# Feature engineering and standardization
prepare_features <- function(dataset) {
  # Encode categorical variables
  for (col_name in colnames(dataset)) {
    if (is.factor(dataset[[col_name]]) || is.character(dataset[[col_name]])) {
      dataset[[col_name]] <- as.numeric(factor(dataset[[col_name]]))
    }
  }
  
  # Standardize numeric variables
  numeric_cols <- sapply(dataset, is.numeric)
  dataset[numeric_cols] <- scale(dataset[numeric_cols])
  
  return(dataset)
}






##################     4. MODELING     ##################

train_test_model <- function(dataset, train_start, train_end, test_start, test_end) {
  # Prepare date ranges
  train_data <- dataset[dataset$Date >= as.Date(train_start) & 
                          dataset$Date <= as.Date(train_end), ]
  test_data <- dataset[dataset$Date >= as.Date(test_start) & 
                         dataset$Date <= as.Date(test_end), ]
  
  # Prepare features
  train_features <- train_data[, !(names(train_data) %in% "Deaths")]
  train_target <- train_data$Deaths
  
  # Train model
  model <- lm(train_target ~ ., data = train_features)
  
  # Make predictions
  test_features <- test_data[, !(names(test_data) %in% "Deaths")]
  predictions <- predict(model, newdata = test_features)
  
  # Evaluate results
  results <- data.frame(
    Date = test_data$Date,
    Actual = test_data$Deaths,
    Predicted = predictions
  )
  
  # Calculate metrics
  metrics <- list(
    R2 = summary(model)$r.squared,
    RMSE = sqrt(mean((results$Actual - results$Predicted)^2)),
    MAE = mean(abs(results$Actual - results$Predicted))
  )
  
  return(list(model = model, results = results, metrics = metrics))
}







################## 5.  EXECUTION ##################

# Read and prepare data
dataset1 <- read.csv("covid_19_india.csv")
dataset2 <- read.csv("COVID-19_Cases.csv")

# Initial data cleanup
dataset1 <- subset(dataset1, select = -Sno)
dataset2 <- subset(dataset2, select = -`S. No.`)

# Handle missing data
dataset1 <- handle_missing_data(dataset1)
dataset2 <- handle_missing_data(dataset2)

# Merge datasets
merged_dataset <- merge(dataset1, dataset2, by = "Date", all = TRUE)

# Clean and prepare final dataset
merged_dataset <- handle_missing_data(merged_dataset)
merged_dataset <- handle_outliers(merged_dataset)
merged_dataset <- prepare_features(merged_dataset)

# Create visualizations
create_visualizations(merged_dataset)

# Train and evaluate model
model_results <- train_test_model(
  merged_dataset,
  train_start = "2020-01-22",
  train_end = "2020-01-28",
  test_start = "2020-01-29",
  test_end = "2020-02-04"
)

# Print results
print(model_results$metrics)







################## 6. REPORTING FUNCTIONS ##################

generate_analysis_plots <- function(data) {
  # Time series plot of deaths
  deaths_plot <- ggplot(data, aes(x = Date, y = Deaths)) +
    geom_line(color = "blue") +
    geom_point(color = "red", size = 2) +
    theme_minimal() +
    labs(title = "COVID-19 Deaths Over Time",
         x = "Date",
         y = "Number of Deaths")
  
  # Distribution of deaths
  death_dist_plot <- ggplot(data, aes(x = Deaths)) +
    geom_histogram(fill = "skyblue", color = "black") +
    theme_minimal() +
    labs(title = "Distribution of COVID-19 Deaths",
         x = "Number of Deaths",
         y = "Frequency")
  
  return(list(deaths_plot = deaths_plot, death_dist_plot = death_dist_plot))
}

create_html_report <- function(data, model_results, plots) {
  # Create R Markdown template
  rmd_content <- '
---
title: "COVID-19 Deaths Analysis in India"
date: "`r format(Sys.time(), "%d %B, %Y")`"
output: 
  html_document:
    theme: cosmo
    toc: true
    toc_float: true
    code_folding: hide
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
```

# Executive Summary

This report presents a comprehensive analysis of COVID-19 deaths in India, including data exploration, statistical analysis, and predictive modeling results.

## 1. Data Overview

### 1.1 Dataset Dimensions
```{r}
dim_data <- dim(data)
cat("Number of rows:", dim_data[1], "\nNumber of columns:", dim_data[2])
```

### 1.2 Summary Statistics
```{r}
summary_stats <- summary(data)
knitr::kable(as.data.frame(summary_stats), caption = "Summary Statistics")
```

## 2. Data Visualization

### 2.1 Time Series Analysis
```{r, fig.width=10, fig.height=6}
print(plots$deaths_plot)
```

### 2.2 Distribution Analysis
```{r, fig.width=10, fig.height=6}
print(plots$death_dist_plot)
```

### 2.3 Correlation Analysis
```{r, fig.width=10, fig.height=8}
numeric_data <- data[, sapply(data, is.numeric)]
correlation_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(correlation_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45)
```

## 3. Model Results

### 3.1 Model Performance Metrics
```{r}
metrics_df <- data.frame(
  Metric = names(model_results$metrics),
  Value = unlist(model_results$metrics)
)
knitr::kable(metrics_df, caption = "Model Performance Metrics")
```

### 3.2 Predictions vs Actual Values
```{r}
results_df <- head(model_results$results, 10)
knitr::kable(results_df, caption = "Sample of Predicted vs Actual Values")
```

### 3.3 Model Visualization
```{r, fig.width=10, fig.height=6}
ggplot(model_results$results, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  theme_minimal() +
  labs(title = "Actual vs Predicted Deaths",
       y = "Number of Deaths",
       color = "Legend") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))
```

## 4. Key Findings

1. The model achieved an R-squared value of `r round(model_results$metrics$R2, 3)`, indicating that `r round(model_results$metrics$R2 * 100, 1)`% of the variance in deaths is explained by our predictors.

2. The Root Mean Square Error (RMSE) of `r round(model_results$metrics$RMSE, 2)` suggests the average prediction error in number of deaths.

3. The Mean Absolute Error (MAE) of `r round(model_results$metrics$MAE, 2)` provides the average absolute difference between predicted and actual deaths.

## 5. Conclusions and Recommendations

Based on the analysis, we can conclude:

1. The time series analysis shows [interpretation of time series trends]
2. The correlation analysis reveals [key correlations found]
3. The predictive model demonstrates [model performance interpretation]

Recommendations:
1. [First recommendation based on findings]
2. [Second recommendation based on findings]
3. [Third recommendation based on findings]
'

# Write the R Markdown content to a file
writeLines(rmd_content, "covid_analysis_report.Rmd")

# Render the report
rmarkdown::render("covid_analysis_report.Rmd", 
                  output_file = "covid_analysis_report.html",
                  params = list(data = data,
                                model_results = model_results,
                                plots = plots))
}



################## 6. MAIN EXECUTION ##################

# [Previous main execution code remains the same up to model training]

# Generate plots for the report
analysis_plots <- generate_analysis_plots(merged_dataset)

# Create the HTML report
create_html_report(merged_dataset, model_results, analysis_plots)

# Print confirmation message
cat("\nAnalysis complete. The report has been generated as 'covid_analysis_report.html'")
