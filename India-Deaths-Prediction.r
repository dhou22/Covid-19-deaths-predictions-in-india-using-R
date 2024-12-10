##############################   Importing Libraries   ##############################

library(readr)

##############################   Loading datasets   ##############################

dataset2 <- read_delim("C:\\Users\\user\\Desktop\\4éme\\stats\\project\\COVID-19 Cases(22-04-2021).csv", delim = ";")
dataset1 <- read_delim("C:\\Users\\user\\Desktop\\4éme\\stats\\project\\covid_19_india.csv", delim = ";")
# Now perform the merge with more control
merged_data <- merge(data1, data2, 
                     by = "Date",
                     all = FALSE)  # Change to FALSE to only keep matching dates

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

data_overview(merged_data)
##############################   Data Exploration   ##############################
##############################   Univariate analysis (Data distribution)   ##############################

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

plot_histograms(merged_data)

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
bivariate_analysis(merged_data)

##############################   Multivariate Analysis   ##############################

install.packages("ggplot2")
install.packages("reshape2")

library(ggplot2)
library(reshape2)
library(ggplot2)
library(reshape2)
# Load the necessary libraries
library(ggplot2)
library(reshape2)

# Step 1: Compute the correlation matrix for dataset
# Ensure you only use numeric columns for the correlation
cor_matrix <- cor(merged_data[, sapply(merged_data, is.numeric)], use = "complete.obs")

# Step 2: Reshape the correlation matrix for dataset using melt()
melted_cor_matrix<- melt(merged_data)

# Step 3: Create a heatmap for dataset using ggplot2
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Matrix Heatmap for Dataset1", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#***************************************************data preperation*****************************

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

# Apply the outlier replacement to numeric columns
merged_data[] <- lapply(merged_data, replace_outliers)

# Visualize the dataset for anomalies
summary(merged_data)
boxplot(merged_data[, sapply(merged_data, is.numeric)], main = "Boxplot After Cleaning", las = 2)


# Compute the correlation matrix for the merged data
cor_matrix <- cor(merged_data[, sapply(merged_data, is.numeric)], use = "complete.obs")

# Reshape the correlation matrix for visualization
melted_cor_matrix <- melt(cor_matrix)

# Plot the heatmap for correlation matrix
library(ggplot2)
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Matrix Heatmap", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#regression model

# Exemple de régression linéaire pour prédire les décès en fonction des cas confirmés et actifs

model <- lm(Deaths ~ `Confirmed` + `Active Cases` + Cured + `Cured/Discharged`, data=merged_data)
summary(model)



# Predict deaths based on the model
merged_data$Predicted_Deaths <- predict(model, newdata = merged_data)




