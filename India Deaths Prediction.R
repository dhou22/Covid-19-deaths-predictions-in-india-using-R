##############################   Importing Libraries   ##############################

library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)
library(lubridate)

# install.packages("ggplot2")
# install.packages("reshape2")
##############################   Loading datasets   ##############################

dataset1 <- read.csv("covid_19_india.csv", sep = ";")
dataset2 <- read.csv("COVID-19_Cases.csv", sep = ";")



##############################   Data Understanding   ##############################
##############################   Data Overview   ##############################

data_overview <- function(data) {
  cat("Structure of the dataset:\n")
  str(data)
  cat("\n")
  
  cat("Dimensions (Rows, Columns): ", dim(data), "\n\n")
  
  cat("Column Names:\n")
  print(names(data))
  cat("\n")
  
  cat("Missing Values by Column:\n")
  missing_vals <- sapply(data, function(x) sum(is.na(x)))
  print(missing_vals)
  cat("\n")
  
  cat("Data Types of Columns:\n")
  data_types <- sapply(data, class)
  print(data_types)
  cat("\n")
  
  cat("Summary Statistics:\n")
  print(summary(data))
  cat("\n")
}




# Standardize the Date format in dataset1
#dataset1$Date <- as.Date(dataset1$Date, format = "%m/%d/%Y")  # Adjust format based on dataset1
# Standardize the Date format in dataset2
#dataset2$Date <- as.Date(dataset2$Date, format = "%d/%m/%Y")  # Adjust format based on dataset2


data_overview(dataset1)
data_overview(dataset2)
#data_overview(merged_dataset)
##############################   Data Exploration   ##############################
# Deleting ID column
dataset1 <- subset(dataset1, select = -`S..No.`)
dataset2 <- dataset2[, -1]  # Removes the first column



##############################   Univariate analysis (Data distribution)   ##############################

# Histograms
plot_histograms <- function(data) {
  # Identify numeric columns
  numeric_columns <- sapply(data, is.numeric)
  
  # Count the number of numeric columns
  num_numeric_columns <- sum(numeric_columns)
  
  # Set up the layout: e.g., 2 rows and 2 columns if you have 4 numeric columns
  par(mfrow = c(ceiling(num_numeric_columns / 2), 2))  # Adjust 2 for the number of columns per row
  
  # Loop through each numeric column and plot histogram
  for (col in names(data)[numeric_columns]) {
    hist(data[[col]], 
         main = paste("Histogram of", col), 
         xlab = col, 
         col = "skyblue", 
         border = "white")
  }
  
  # Reset plotting layout
  par(mfrow = c(1, 1))
}

#plot_histograms(dataset1)
#plot_histograms(dataset2)

# Boxplots

create_boxplots <- function(dataset, title = "Boxplots of Numeric Features") {
  # Extract numeric columns
  numeric_data <- dataset[, sapply(dataset, is.numeric)]
  
  # Reshape the data for ggplot2
  melted_data <- reshape2::melt(numeric_data, variable.name = "Feature", value.name = "Value")
  
  # Create the boxplots
  boxplots <- ggplot(melted_data, aes(x = Feature, y = Value)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
    theme_minimal() +
    labs(title = title, x = "Features", y = "Values") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(boxplots)
}

#create_boxplots(dataset1)
#create_boxplots(dataset2)




##############################   Bivariate Analysis   ##############################

bivariate_analysis <- function(data) {
  # Get the column names and data types
  columns <- names(data)
  column_types <- sapply(data, class)
  
  # Set up a fixed 2x2 layout for displaying four plots at a time
  par(mfrow = c(2, 2))
  
  plot_count <- 0
  
  # Loop through each pair of columns
  for (i in 1:(length(columns) - 1)) {
    for (j in (i + 1):length(columns)) {
      col1 <- columns[i]
      col2 <- columns[j]
      
      # Determine the types of the two columns
      type1 <- column_types[i]
      type2 <- column_types[j]
      
      # Numeric-Numeric Analysis
      if (type1 == "numeric" && type2 == "numeric") {
        plot(data[[col1]], data[[col2]], main = paste("Scatter Plot:", col1, "vs", col2),
             xlab = col1, ylab = col2, col = "blue", pch = 16)
        
        # Numeric-Categorical Analysis
      } else if ((type1 == "numeric" && type2 == "factor") || (type1 == "factor" && type2 == "numeric")) {
        if (type1 == "factor") {
          boxplot(data[[col2]] ~ data[[col1]], main = paste("Boxplot of", col2, "by", col1),
                  xlab = col1, ylab = col2, col = "lightblue")
        } else {
          boxplot(data[[col1]] ~ data[[col2]], main = paste("Boxplot of", col1, "by", col2),
                  xlab = col2, ylab = col1, col = "lightblue")
        }
        
        # Categorical-Categorical Analysis
      } else if (type1 == "factor" && type2 == "factor") {
        table_data <- table(data[[col1]], data[[col2]])
        mosaicplot(table_data, main = paste("Mosaic Plot:", col1, "and", col2),
                   xlab = col1, ylab = col2, col = c("skyblue", "pink"))
      }
      
      # Increment plot count and reset layout after four plots
      plot_count <- plot_count + 1
      if (plot_count %% 4 == 0) {
        par(ask = TRUE)  # Pause after every set of four plots
        par(mfrow = c(2, 2))  # Reset layout for the next set of plots
      }
    }
  }
  
  # Reset layout to single plot
  par(mfrow = c(1, 1))
}

#bivariate_analysis(dataset1)	
#bivariate_analysis(dataset2)




##############################   Multivariate Analysis   ##############################

create_correlation_heatmap <- function(dataset, title = "Correlation Matrix Heatmap") {
  # Step 1: Compute the correlation matrix for numeric columns
  cor_matrix <- cor(dataset[, sapply(dataset, is.numeric)], use = "complete.obs")
  
  # Step 2: Reshape the correlation matrix using melt()
  melted_cor_matrix <- melt(cor_matrix)
  
  # Step 3: Create and return a heatmap
  heatmap <- ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
    theme_minimal() +
    labs(title = title, x = "", y = "") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(heatmap)
}
#create_correlation_heatmap(dataset1)
#create_correlation_heatmap(dataset2)



##############################   Check for outliers    ##################################

show_outliers_boxplot <- function(dataset) {
  # Step 1: Identify numeric columns
  numeric_columns <- dataset[, sapply(dataset, is.numeric)]
  
  # Create an empty list to store boxplots
  boxplots_list <- list()
  
  # Step 2: Iterate over numeric columns to create boxplots
  for (col in colnames(numeric_columns)) {
    # Calculate the statistics to identify outliers
    column_data <- numeric_columns[[col]]
    
    # Identify outliers using IQR method
    Q1 <- quantile(column_data, 0.25, na.rm = TRUE)
    Q3 <- quantile(column_data, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    outliers <- column_data[column_data < lower_bound | column_data > upper_bound]
    
    # Step 3: Create boxplot with outliers highlighted
    boxplot <- ggplot(dataset, aes(x = factor(1), y = .data[[col]])) +  # Use .data[[col]] to handle special characters
      geom_boxplot(outlier.colour = "red", outlier.size = 3) +  # Boxplot with red outliers
      geom_point(data = dataset %>% filter(.data[[col]] %in% outliers), 
                 aes(x = factor(1), y = .data[[col]]), color = "blue", size = 3) +
      labs(title = paste("Boxplot for", col), x = col, y = "Values") +
      theme_minimal() +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank())  # Remove x-axis labels for better presentation
    
    # Add the boxplot to the list
    boxplots_list[[col]] <- boxplot
  }
  
  # Step 4: Arrange the boxplots side by side
  do.call(grid.arrange, c(boxplots_list, ncol = 3))  # Adjust ncol as per your layout preference
}

#show_outliers_boxplot(dataset1)
#show_outliers_boxplot(dataset2)




##############################   Data Preparation   ##############################


############################   Handeling Mising data   ###########################
handling_missing_data <- function(dataset) {
  # For each column in the dataset
  for (col_name in colnames(dataset)) {
    # Calculate the median (this works for both numeric and categorical types)
    # For numeric columns, use the median directly
    if (is.numeric(dataset[[col_name]])) {
      median_value <- median(dataset[[col_name]], na.rm = TRUE)
    } else {
      # For non-numeric (categorical) columns, calculate the most frequent value (mode)
      mode_value <- names(sort(table(dataset[[col_name]]), decreasing = TRUE))[1]
      median_value <- mode_value
    }
    
    # Replace missing values with the calculated median or mode
    dataset[[col_name]][is.na(dataset[[col_name]])] <- median_value
  }
  return(dataset)
}
dataset2 <-handling_missing_data(dataset2)
dataset1 <-handling_missing_data(dataset1)



############################   Merging datasets   ###########################

merged_data
merged_dataset <- merge(dataset1, dataset2, by = "Date", all = TRUE)
merged_dataset <-handling_missing_data(merged_dataset)




############################   Handeling Outliers   ###########################

handling_outliers <- function(dataset) {
  # Loop twice to handle outliers in the dataset
  for (i in 1:2) {
    # Print the mean of each numeric column before processing outliers
    cat("Mean of columns before loop", i, ":\n")
    numeric_columns <- dataset[, sapply(dataset, is.numeric)]  # Select only numeric columns
    print(colMeans(numeric_columns, na.rm = TRUE))
    
    # For each column in the dataset
    for (col_name in colnames(dataset)) {
      # If the column is numeric
      if (is.numeric(dataset[[col_name]])) {
        # Calculate the IQR for the column
        Q1 <- quantile(dataset[[col_name]], 0.25, na.rm = TRUE)
        Q3 <- quantile(dataset[[col_name]], 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        
        # Identify the lower and upper bounds for outliers
        lower_bound <- Q1 - 1.5 * IQR
        upper_bound <- Q3 + 1.5 * IQR
        
        # Replace outliers with the mean value of the column
        dataset[[col_name]][dataset[[col_name]] < lower_bound | dataset[[col_name]] > upper_bound] <- mean(dataset[[col_name]], na.rm = TRUE)
      }
    }
  }
  
  # Return the dataset after handling outliers
  return(dataset)
}
show_outliers_boxplot(merged_dataset)
merged_dataset <- handling_outliers(merged_dataset)
show_outliers_boxplot(merged_dataset)



############################   Encoding   ###########################
encode_features <- function(dataset) {
  for (col_name in colnames(dataset)) {
    # Exclure la colonne "Date"
    if (col_name != "Date" && (is.factor(dataset[[col_name]]) || is.character(dataset[[col_name]]))) {
      # Créer un mapping des valeurs uniques vers des labels numériques
      unique_values <- unique(dataset[[col_name]])
      label_map <- setNames(seq_along(unique_values), unique_values)
      
      # Remplacer les valeurs catégoriques par leurs labels numériques
      dataset[[col_name]] <- as.numeric(label_map[dataset[[col_name]]])
    }
  }
  return(dataset)
}
#rm(merged_dataset_encoded)
# Apply the function to encode features
merged_dataset_encoded <- encode_features(merged_dataset)

str(merged_dataset_encoded)




############################   Feature selection   ###########################

delete_collinear_features <- function(dataset, threshold = 0.8) {
  # Step 1: Compute the correlation matrix for numeric columns
  cor_matrix <- cor(dataset[, sapply(dataset, is.numeric)], use = "complete.obs")
  
  # Step 2: Identify highly correlated feature pairs (absolute correlation > threshold)
  high_cor_pairs <- which(abs(cor_matrix) > threshold, arr.ind = TRUE)
  
  # Avoid self-correlations and retain only one direction of the pair
  high_cor_pairs <- high_cor_pairs[high_cor_pairs[, 1] < high_cor_pairs[, 2], ]
  
  # Step 3: Create a list of features to remove
  features_to_remove <- unique(colnames(dataset)[high_cor_pairs[, 2]])
  
  # Step 4: Remove the selected features from the dataset
  reduced_dataset <- dataset[, !colnames(dataset) %in% features_to_remove]
  
  # Step 5: Print details of removed features (optional)
  cat("Removed features due to high correlation:", features_to_remove, "\n")
  
  return(reduced_dataset)
}

# Example usage:
merged_dataset_encoded <- delete_collinear_features(merged_dataset_encoded, threshold = 0.9)
write.csv(merged_dataset_encoded, "merged_dataset_encoded.csv", row.names = TRUE)




################################# Extracting week 4 and 5 data#######################
###########   Linear Regression (1-4th -> 5th week of 2020 dataset)   ########
#rm(week_4_data)
#rm(week_5_data)


# Filter data for week 4 (training data)
week_4_data <-  merged_dataset_encoded [ merged_dataset_encoded $Date >= as.Date("01/02/2021") & 
                                           merged_dataset_encoded $Date <= as.Date("28/02/2021"), ]
x <-  week_4_data
week_4_data
week_4_data <- na.omit(week_4_data)  # Remove rows with NAs

# Filter data for week 5 (test data)
week_5_data <- merged_dataset_encoded [ merged_dataset_encoded $Date >= as.Date("01/02/2021") & 
                                          merged_dataset_encoded $Date <= as.Date("07/02/2021"),]

week_5_data <- na.omit(week_5_data)  # Remove rows with NAs




############################   Standardization   ###########################

# Function to standardize numeric columns in the dataset (z-score normalization)
standardize_dataset <- function(dataset) {
  # Loop through each column in the dataset
  for (col_name in colnames(dataset)) {
    # Check if the column is numeric
    if (is.numeric(dataset[[col_name]])) {
      # Standardize the column (z-score normalization)
      mean_value <- mean(dataset[[col_name]], na.rm = TRUE)
      sd_value <- sd(dataset[[col_name]], na.rm = TRUE)
      
      # Avoid division by zero if the standard deviation is zero
      if (sd_value != 0) {
        dataset[[col_name]] <- (dataset[[col_name]] - mean_value) / sd_value
      } else {
        dataset[[col_name]] <- 0  # Assign 0 if standard deviation is zero (no variation)
      }
    }
  }
  return(dataset)
}

# Assuming merged_dataset_encoded is already loaded/defined
standardized_dataset <- standardize_dataset(merged_dataset_encoded)

# Print the first few rows of the standardized dataset to verify
print(head(standardized_dataset))

# Create a correlation heatmap (if the function is defined elsewhere)
create_correlation_heatmap(standardized_dataset)




############################   Modeling   ###########################
# Data filtering 
# Ensure the 'Date' column is in Date format for all datasets
dataset1$Date <- as.Date(dataset1$Date, format = "%d/%m/%Y")
dataset2$Date <- as.Date(dataset2$Date, format = "%d/%m/%Y")
merged_dataset_encoded$Date <- as.Date(merged_dataset_encoded$Date, format = "%d/%m/%Y")

# Filter Data for Weeks 4, 5, and 6
week_4_data <- merged_dataset_encoded %>%
  filter(Date >= as.Date("2021-02-01") & Date <= as.Date("2021-02-28")) %>%
  na.omit()

week_5_data <- merged_dataset_encoded %>%
  filter(Date >= as.Date("2021-03-01") & Date <= as.Date("2021-03-07")) %>%
  na.omit()

week_6_data <- merged_dataset_encoded %>%
  filter(Date >= as.Date("2021-03-08") & Date <= as.Date("2021-03-14")) %>%
  na.omit()

# Standardize data
# Standardize Week 4 Data (Training)
week_4_data <- standardize_dataset(week_4_data)

# Standardize Week 5 and Week 6 Data using Week 4 mean and std
week_5_data <- standardize_dataset(week_5_data)
week_6_data <- standardize_dataset(week_6_data)

# Prepare for model training 
# Extract target variable (Deaths) from Week 4 Data
week_4_output <- week_4_data$Death  # The target variable

# Prepare input features by removing 'Deaths' column
week_4_input <- week_4_data[, !(names(week_4_data) %in% "Death")]
week_4_input <- week_4_input[, colSums(is.na(week_4_input)) < nrow(week_4_input)]  # Remove columns with all NAs

# Train the Linear Regression Model
linear_model <- lm(week_4_output ~ ., data = week_4_input)  # 'Deaths' as the target

# Prepare Week 5 Data for Predictions
week_5_input <- week_5_data[, !(names(week_5_data) %in% "Death")]
week_5_input <- week_5_input[, colSums(is.na(week_5_input)) < nrow(week_5_input)]  # Remove columns with all NAs

# Make predictions using the trained model
predictions <- predict(linear_model, newdata = week_5_input)

# Compare the predicted deaths with the actual deaths for Week 5
results <- data.frame(Actual = week_5_data$Death, Predicted = predictions)
print(head(results))  # Display a few results

# Summarize model performance
cat("Linear Model R-squared: ", summary(linear_model)$r.squared, "\n")
cat("Adjusted R-squared: ", summary(linear_model)$adj.r.squared, "\n")

# ANOVA for Linear Model
anova_result <- anova(linear_model)
print(anova_result)

# Function to train models (RLM, RF) using Week 4 data
train_models <- function(data) {
  # Split data into training and testing sets
  train_idx <- data$Week <= 4
  train_data <- data[train_idx, ]
  test_data <- data[!train_idx & data$Week <= 6, ]
  
  # Prepare formula
  predictors <- names(data)[!names(data) %in% c("Date", "Week", "Deaths")]
  formula <- as.formula(paste("Deaths ~", paste(predictors, collapse = " + ")))
  
  # Train models
  set.seed(123)
  
  # Robust Linear Model
  model_rlm <- rlm(formula, data = train_data)
  
  # Random Forest with cross-validation
  ctrl <- trainControl(method = "cv", number = 5)
  model_rf <- train(
    formula,
    data = train_data,
    method = "rf",
    trControl = ctrl,
    ntree = 500
  )
  
  return(list(
    models = list(RLM = model_rlm, RF = model_rf),
    train_data = train_data,
    test_data = test_data
  ))
}

# Call the function to train models using Week 4 data
models_result <- train_models(merged_dataset_encoded)

# Evaluate the Random Forest model
rf_predictions <- predict(models_result$models$RF, newdata = models_result$test_data)
rf_results <- data.frame(Actual = models_result$test_data$Deaths, Predicted = rf_predictions)
print(head(rf_results))  # Display a few results




##################################################################################
# Reverse standardization for predictions
#destandarization fct----> IT IS NOT WORKING !!!
destandarize_dataset <- function(dataset, original_means, original_sds) {
  # Ensure that the dataset and original means/standard deviations have matching column names
  valid_columns <- intersect(names(dataset), names(original_means))
  
  if(length(valid_columns) == 0) {
    stop("No matching columns found between dataset and provided means/sds.")
  }
  
  # Filter dataset to keep only the valid columns
  dataset <- dataset[, valid_columns]
  
  for (col_name in valid_columns) {
    # Retrieve original mean and standard deviation
    mean_value <- original_means[col_name]
    sd_value <- original_sds[col_name]
    
    # Reverse standardization
    dataset[[col_name]] <- (dataset[[col_name]] * sd_value) + mean_value
  }
  
  return(dataset)
}
# Assuming 'results', 'original_means', and 'original_sds' are already defined and standardized
destandarized_results <- destandarize_dataset(results, original_means, original_sds)

# Print the destandardized results
print(head(destandardized_results))



###########   Linear Regression (4th -> 6th week of 2020 dataset)   ########
# Make sure the Date column is in the Date format (if not, convert it)


# Filter data for week 6 (test data)
week_6_data <- standardized_dataset[standardized_dataset$Date >= as.Date("05/03/2020") & 
                                      standardized_dataset$Date <= as.Date("2020-02-11"), ]
week_6_data <- na.omit(week_6_data)  # Remove rows with NAs

# Train the linear regression model using week 4 data
week_4_input <- week_4_data[, !(names(week_4_data) %in% "Deaths")]  # All columns except Deaths
week_4_output <- week_4_data$Deaths  # Deaths column from week 4 (target variable)

linear_model <- lm(week_4_output ~ ., data = week_4_input)  # Train the model

# Test the model on week 6 data (to predict Deaths for week 6)
week_6_input <- week_6_data[, !(names(week_6_data) %in% "Deaths")]  # Features for week 6 (no Deaths column)
predictions <- predict(linear_model, newdata = week_6_input)  # Predict Deaths for week 6

# Compare the predictions with the actual Deaths in week 6
results <- data.frame(Actual = week_6_data$Deaths, Predicted = predictions)

# Print the first few results
head(results)

# Print the sum of the predicted values
sum_predicted <- sum(predictions)
print(paste("Sum of predicted values:", sum_predicted))

# Print the sum of the actual values
sum_actual <- sum(week_6_data$Deaths)
print(paste("Sum of actual values:", sum_actual))





###################################################
# Ensure date column consistency
dataset1$Date <- as.Date(dataset1$Date, format = "%d/%m/%Y")
dataset2$Date <- as.Date(dataset2$Date, format = "%d/%m/%Y")
merged_dataset_encoded$Date <- as.Date(merged_dataset_encoded$Date, format = "%d/%m/%Y")


# Filter Week 4 Data
week_4_data <- merged_dataset_encoded %>%
  filter(Date >= as.Date("2021-02-01") & Date <= as.Date("2021-02-28")) %>%
  na.omit()

# Filter Week 6 Data
week_6_data <- merged_dataset_encoded %>%
  filter(Date >= as.Date("2021-03-05") & Date <= as.Date("2021-03-11")) %>%
  na.omit()

# Standardize Data
week_4_data <- standardize_dataset(week_4_data)
week_6_data <- standardize_dataset(week_6_data)

# Model Training (Week 4 -> Week 5)
week_4_output <- week_4_data$Death
week_4_input <- week_4_data[, !(names(week_4_data) %in% "Death")]
linear_model <- lm(week_4_output ~ ., data = week_4_data)

# Predictions for Week 6
week_6_input <- week_6_data[, !(names(week_6_data) %in% "Death")]
predictions <- predict(linear_model, newdata = week_6_input)

# Compare Predictions
results <- data.frame(Actual = week_6_data$Deaths, Predicted = predictions)
print(head(results))

# Visualization
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(color = 'blue') +
  geom_abline(slope = 1, intercept = 0, color = 'red') +
  labs(title = "Actual vs Predicted Deaths (Week 6)", x = "Actual Deaths", y = "Predicted Deaths")











