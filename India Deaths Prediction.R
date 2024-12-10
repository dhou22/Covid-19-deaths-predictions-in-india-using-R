
#** project : Covid-19 deaths prediction in india ***#

##################         1. DATA UNDERSTANDING        ####################

##  Load required libraries
library(tidyverse)
library(skimr)
library(DataExplorer)
library(corrplot)
library(moments)

##   Function for basic data summary
basic_summary <- function(data) {
  cat("\n=== Basic Data Summary ===\n")
  
  # Dimensions
  cat("\nDimensions:", dim(data))
  
  # Data types
  cat("\n\nData Types:\n")
  print(sapply(data, class))
  
  # Missing values
  cat("\nMissing Values:\n")
  print(colSums(is.na(data)))
  
  # Basic statistics
  cat("\nNumerical Summary:\n")
  print(summary(data))
}



##   Function for detailed numerical analysis
numerical_analysis <- function(data) {
  num_cols <- sapply(data, is.numeric) #check if each column is numeric(true/false) and create a logical vector num_cols
  num_data <- data[, num_cols] #select only the numeric columns (num_cols=true)
  
  cat("\n=== Numerical Analysis ===\n")
  
  # Detailed statistics
  stats_df <- data.frame(
    Mean = sapply(num_data, mean, na.rm = TRUE), #calculates the mean of each column +ignoring missing values 
    Median = sapply(num_data, median, na.rm = TRUE), #calculates the median of each column 
    SD = sapply(num_data, sd, na.rm = TRUE), #sd:Standard Deviation measures how spread out the numbers are from their average 
    Skewness = sapply(num_data, skewness, na.rm = TRUE), # Skewness measures the asymmetry of data distribution
    Kurtosis = sapply(num_data, kurtosis, na.rm = TRUE), # kurtosis mesures how much extreme values we have+ identifying whether we have more outliers than a normal distribution 
    Q1 = sapply(num_data, quantile, probs = 0.25, na.rm = TRUE), # 25% of num_data values are under q1 
    Q3 = sapply(num_data, quantile, probs = 0.75, na.rm = TRUE)  # 75% of num_data values  are under q2 
  )
  
  print(stats_df)
  
  
  # Visualize correlation matrix

  cor_matrix <- cor(num_data, use = "complete.obs")
  
  corrplot(cor_matrix,          # the correlation matrix to plot
           method = "color",    # use colors to show correlations
           type = "upper",      # show only upper triangle
           addCoef.col = "black", # add correlation coefficients in black
           tl.col = "black",    # make text labels black
           tl.srt = 45)        # rotate text labels 45 degrees
}



##   Function for categorical analysis
categorical_analysis <- function(data) {
  
  cat_cols <- sapply(data, is.character) | sapply(data, is.factor)
  cat_data <- data[, cat_cols]
  
  cat("\n=== Categorical Analysis ===\n")
  
  for(col in names(cat_data)) {
    
    cat("\nFrequency table for", col, ":\n")
    print(table(cat_data[[col]], useNA = "ifany"))
    
    cat("\nPercentage distribution for", col, ":\n")
    print(round(prop.table(table(cat_data[[col]], useNA = "ifany")) * 100, 2))
  }
}



##   Function for distribution analysis 
distribution_analysis <- function(data) {
  
  num_cols <- sapply(data, is.numeric)
  num_data <- data[, num_cols]
  
  #create histogram and density plots 
  par(mfrow = c(2,ceiling(ncol(num_data)/2)))
  for (col in names(num_data)) {
    hist(num_data[[col]], main = paste("Histogram of", col),
         prob = TRUE, col = "lightblue")
    lines(density(num_data[[col]], na.rm = TRUE), col = "red")
  }
  
  par(mfrow = c(1,1))
  
  
  # Create Q-Q plots
  par(mfrow = c(2, ceiling(ncol(num_data)/2)))
  for(col in names(num_data)) {
    qqnorm(num_data[[col]], main = paste("Q-Q Plot of", col))
    qqline(num_data[[col]], col = "red")
  }
  par(mfrow = c(1,1))
}



##   Function for outlier detection
outlier_analysis <- function(data) {
  num_cols <- sapply(data, is.numeric)
  num_data <- data[, num_cols]
  
  cat("\n=== Outlier Analysis ===\n")
  
  for(col in names(num_data)) {
    q1 <- quantile(num_data[[col]], 0.25, na.rm = TRUE)
    q3 <- quantile(num_data[[col]], 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    
    outliers <- sum(num_data[[col]] < lower_bound | num_data[[col]] > upper_bound, na.rm = TRUE)
    
    cat("\nOutliers in", col, ":", outliers, "\n")
    cat("Lower bound:", lower_bound, "\n")
    cat("Upper bound:", upper_bound, "\n")
  }
  
  
  
  # Create boxplots
  boxplot(num_data, main = "Boxplots for Numerical Variables",
          las = 2, cex.axis = 0.7)
}







####################        2. DATA PREPARATION       ######################


# Read the CSV files
data1 <- read.csv(file = "covid_19_india.csv")
data2 <- read.csv(file = "COVID-19_Cases.csv")

# Convert dates using the correct format for each dataset
data1$Date <- as.Date(data1$Date, format = "%Y-%m-%d")  # For dates like "2020-01-30"
data2$Date <- as.Date(data2$Date, format = "%d/%m/%Y")  # For dates like "12/03/2020"

# Verify the conversion worked
print("Date range in data1:")
print(range(data1$Date, na.rm = TRUE))

print("Date range in data2:")
print(range(data2$Date, na.rm = TRUE))

# Now perform the merge with properly formatted dates
merged_data <- merge(data1, data2, 
                     by = "Date",
                     all = FALSE)  # Only keep matching dates

# Check the results
print("Number of rows in each dataset:")
print(paste("data1 rows:", nrow(data1)))
print(paste("data2 rows:", nrow(data2)))
print(paste("merged_data rows:", nrow(merged_data)))


# Check for missing values in each column
missing_values <- sapply(merged_data, function(col) sum(is.na(col)))
print(missing_values)  # Print the count of missing values per column

# Percentage of missing values
missing_percent <- sapply(merged_data, function(col) mean(is.na(col))) * 100
print(missing_percent)
# Replace missing values in numeric columns with the mean 
merged_data[] <- lapply(merged_data, function(col) {
  if (is.numeric(col)) {
    col[is.na(col)] <- mean(col, na.rm = TRUE)
  }
  return(col)
})
# Remplacer les valeurs manquantes ou incorrectes par NA
merged_data[merged_data == "-"] <- NA

# Vérification après nettoyage
head(merged_data)


#***cheking and handling outlliers****
## Boxplot 
numeric_columns <- merged_data[, sapply(merged_data, is.numeric)]

# Create boxplot for all numeric columns in dataset1
boxplot(numeric_columns, 
        main = "Boxplot for Outliers in Numeric Columns (dataset1)", 
        las = 2,  # Rotate axis labels
        col = "skyblue", 
        border = "blue")

#remove Sno, and S.no
merged_data<-subset(merged_data,select=-Sno)
# Remove the column "S. no." from merged_data
merged_data <- subset(merged_data, select = -`S. No.`)
#Death and Deaths gives the same info so we eliminate one of them
merged_data <- subset(merged_data, select = -`Death`)


# Replace outliers with the median for numeric columns
replace_outliers <- function(x) {
  if (is.numeric(x)) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    x[x < lower_bound | x > upper_bound] <- median(x, na.rm = TRUE)  # Replace outliers
  }
  return(x)
}

print("Number of completely duplicate rows:")
sum(duplicated(merged_data))


# Run all analyses
basic_summary(merged_data)
numerical_analysis(merged_data)
categorical_analysis(merged_data)
distribution_analysis(merged_data)
outlier_analysis(merged_data)

# Additional useful functions
# Comprehensive data profiling
create_report(merged_data, output_file = "data_report.html")

# Quick summary using skimr
skim(merged_data)

# Plot missing data patterns
plot_missing(data)

  
  