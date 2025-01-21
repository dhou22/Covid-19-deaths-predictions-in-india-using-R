##############################   Importing Libraries   ##############################

library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)
library(lubridate)
install.packages("gridExtra")
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
dataset1 <- subset(dataset1, select = -`Sno`)
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

############################   Standarization   ###########################

standardize_dataset <- function(dataset) {
  # Loop through each column in the dataset
  for (col_name in colnames(dataset)) {
    # Check if the column is numeric
    if (is.numeric(dataset[[col_name]])) {
      # Standardize the column (z-score normalization)
      mean_value <- mean(dataset[[col_name]], na.rm = TRUE)
      sd_value <- sd(dataset[[col_name]], na.rm = TRUE)
      
      # Apply standardization
      dataset[[col_name]] <- (dataset[[col_name]] - mean_value) / sd_value
    }
  }
  return(dataset)
}

merged_dataset_encoded
standardized_dataset <- standardize_dataset(merged_dataset_encoded)
print(head(standardized_dataset))

create_correlation_heatmap(standardized_dataset)
############################   Modeling   ###########################

#data filtering 
# Ensure the 'Date' column is in Date format
dataset1$Date <- as.Date(dataset1$Date, format = "%d/%m/%Y")
dataset2$Date <- as.Date(dataset2$Date, format = "%d/%m/%Y")
merged_dataset_encoded$Date <- as.Date(merged_dataset_encoded$Date, format = "%d/%m/%Y")

# Filter Week 4 Data (Ensure the date range is correct)
week_4_data <- merged_dataset_encoded %>%
  filter(Date >= as.Date("2021-02-01") & Date <= as.Date("2021-02-28")) %>%
  na.omit()

# Filter Week 5 Data
week_5_data <- merged_dataset_encoded %>%
  filter(Date >= as.Date("2021-03-01") & Date <= as.Date("2021-03-07")) %>%
  na.omit()

# Filter Week 6 Data (for testing later)
week_6_data <- merged_dataset_encoded %>%
  filter(Date >= as.Date("2021-03-08") & Date <= as.Date("2021-03-14")) %>%
  na.omit()
#Standardizing data
# Standardize Week 4 Data (Training)
week_4_data <- standardize_dataset(week_4_data)

# Standardize Week 5 and Week 6 Data using Week 4 mean and std
week_5_data <- standardize_dataset(week_5_data)
week_6_data <- standardize_dataset(week_6_data)

rm(week_4_input)
#model training 
# Extract target variable (Deaths) from Week 4 Data
week_4_output <- week_4_data$Death  # The target variable

# Prepare input features by removing 'Deaths' column
week_4_input <- week_4_data[, !(names(week_4_data) %in% "Death")]
# Remove columns where all values are NA
week_4_input <-  week_4_input [, colSums(is.na( week_4_input )) < nrow( week_4_input )]

# Train the Linear Regression Model
linear_model <- lm(week_4_output ~ ., data = week_4_input)  # 'Deaths' as the target
#Predicting and Evaluating Model on Week 5
# Prepare the input features for Week 5 data (remove 'Deaths' column)
week_5_input <- week_5_data[, !(names(week_5_data) %in% "Death")]
week_5_input <-  week_5_input [, colSums(is.na( week_5_input )) < nrow( week_5_input )]

# Make predictions using the trained model
predictions <- predict(linear_model, newdata = week_5_input)

# Compare the predicted deaths with the actual deaths for Week 5
results <- data.frame(Actual = week_5_data$Death, Predicted = predictions)
print(head(results))  # Display a few results
# Summarize model performance
cat("R-squared: ", summary(linear_model)$r.squared, "\n")
cat("Adjusted R-squared: ", summary(linear_model)$adj.r.squared, "\n")

#ANOVA
# Model Summary
summary(linear_model)

# ANOVA
anova_result <- anova(linear_model)
print(anova_result)
##################################################################################
# Function to standardize dataset while storing original parameters
standardize_dataset <- function(dataset) {
  # Create lists to store means and standard deviations
  means <- list()
  sds <- list()
  
  # Create a copy of the dataset to avoid modifying the original
  standardized_data <- dataset
  
  # Loop through each column in the dataset
  for (col_name in colnames(dataset)) {
    # Check if the column is numeric and not a date
    if (is.numeric(dataset[[col_name]]) && !inherits(dataset[[col_name]], "Date")) {
      # Calculate and store mean and standard deviation
      means[[col_name]] <- mean(dataset[[col_name]], na.rm = TRUE)
      sds[[col_name]] <- sd(dataset[[col_name]], na.rm = TRUE)
      
      # Standardize the column
      standardized_data[[col_name]] <- (dataset[[col_name]] - means[[col_name]]) / sds[[col_name]]
    }
  }
  
  # Add the means and sds as attributes to the standardized dataset
  attr(standardized_data, "means") <- means
  attr(standardized_data, "sds") <- sds
  
  return(standardized_data)
}

# Function to destandardize dataset using stored parameters
destandardize_dataset <- function(dataset, reference_dataset = NULL) {
  # If reference_dataset is provided, use its attributes
  if (!is.null(reference_dataset)) {
    means <- attr(reference_dataset, "means")
    sds <- attr(reference_dataset, "sds")
  } else {
    # Otherwise use the dataset's own attributes
    means <- attr(dataset, "means")
    sds <- attr(dataset, "sds")
  }
  
  # Check if means and sds exist
  if (is.null(means) || is.null(sds)) {
    stop("No standardization parameters found. The dataset must be standardized first.")
  }
  
  # Create a copy of the dataset
  destandardized_data <- dataset
  
  # Loop through each column that has stored parameters
  for (col_name in names(means)) {
    if (col_name %in% colnames(dataset)) {
      # Reverse the standardization
      destandardized_data[[col_name]] <- (dataset[[col_name]] * sds[[col_name]]) + means[[col_name]]
    }
  }
  
  return(destandardized_data)
}
results <- data.frame(
  Actual = week_5_data$Death,
  Predicted = predictions
)

# Destandardize the results using Week 4's parameters
destandardized_results <- destandardize_dataset(results, standardized_week4)
# Example usage for the linear regression scenario:
# Standardize training data (Week 4)
standardized_week4 <- standardize_dataset(week_4_data)

# Standardize test data (Week 5) using Week 4's parameters
destandardized_results <- destandardize_dataset(results, standardized_week4)
print(head(destandardized_results))

# Train model and make predictions
# ... [your existing model training code] ...

# Create results dataframe with standardized predictions
results <- data.frame(
  Actual = week_5_data$Death,
  Predicted = predictions
)

# Destandardize the results using Week 4's parameters
destandardized_results <- destandardize_dataset(results, standardized_week4)


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







