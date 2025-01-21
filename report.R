##############################   Importing Libraries   ##############################

library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)
library(lubridate)
library(htmltools)  # For HTML generation
library(base64enc)  # For encoding plots
library(webshot)    # For saving plots as images

##############################   Loading datasets   ##############################

dataset1 <- read.csv("covid_19_india.csv", sep = ";")
dataset2 <- read.csv("COVID-19_Cases.csv", sep = ";")

##############################   Data Understanding   ##############################
##############################   Data Overview   ##############################

data_overview <- function(data) {
  overview <- list()
  
  overview$structure <- capture.output(str(data))
  overview$dimensions <- dim(data)
  overview$column_names <- names(data)
  overview$missing_vals <- sapply(data, function(x) sum(is.na(x)))
  overview$data_types <- sapply(data, class)
  overview$summary <- capture.output(summary(data))
  
  return(overview)
}

##############################   Save Plots as Base64   ##############################

save_plot_as_base64 <- function(plot) {
  temp_file <- tempfile(fileext = ".png")
  ggsave(temp_file, plot, width = 10, height = 6)
  img_data <- base64encode(temp_file)
  unlink(temp_file)
  return(img_data)
}

##############################   Generate Analysis Plots   ##############################

generate_analysis_plots <- function(dataset1, dataset2, merged_dataset) {
  plots <- list()
  
  # Univariate Analysis
  plots$univariate <- create_boxplots(merged_dataset, "Distribution of Key Metrics")
  
  # Bivariate Analysis
  numeric_cols <- sapply(merged_dataset, is.numeric)
  cor_data <- cor(merged_dataset[, numeric_cols], use = "complete.obs")
  plots$correlation <- create_correlation_heatmap(merged_dataset)
  
  # Outliers Analysis
  plots$outliers_before <- show_outliers_boxplot(merged_dataset)
  merged_dataset_cleaned <- handling_outliers(merged_dataset)
  plots$outliers_after <- show_outliers_boxplot(merged_dataset_cleaned)
  
  return(plots)
}

##############################   Generate HTML Report   ##############################

generate_html_report <- function(dataset1, dataset2, merged_dataset, plots) {
  # Convert plots to base64
  plot_data <- lapply(plots, save_plot_as_base64)
  
  # Generate HTML content
  html_content <- sprintf('
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>COVID-19 Data Analysis Report</title>
    <style>
        body {
            font-family: "Segoe UI", Tahoma, Geneva, Verdana, sans-serif;
            line-height: 1.6;
            max-width: 1200px;
            margin: 0 auto;
            padding: 20px;
            color: #333;
        }
        h1, h2, h3 {
            color: #2c3e50;
            border-bottom: 2px solid #eee;
            padding-bottom: 10px;
        }
        .section {
            margin: 40px 0;
            padding: 20px;
            background: #fff;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .visualization {
            margin: 20px 0;
            text-align: center;
        }
        .interpretation {
            background: #f8f9fa;
            padding: 15px;
            border-left: 4px solid #2c3e50;
            margin: 15px 0;
        }
        .insight {
            color: #2980b9;
            font-weight: 500;
        }
        img {
            max-width: 100%%;
            height: auto;
        }
    </style>
</head>
<body>
    <h1>COVID-19 Data Analysis Report</h1>
    
    <div class="section">
        <h2>1. Data Overview</h2>
        <div class="interpretation">
            <h3>Dataset Dimensions:</h3>
            <ul>
                <li>Dataset 1: %d rows, %d columns</li>
                <li>Dataset 2: %d rows, %d columns</li>
                <li>Merged Dataset: %d rows, %d columns</li>
            </ul>
        </div>
    </div>

    <div class="section">
        <h2>2. Distribution Analysis</h2>
        <div class="visualization">
            <img src="data:image/png;base64,%s" alt="Distribution Analysis">
            <p class="insight">Distribution of key COVID-19 metrics showing patterns and outliers.</p>
        </div>
    </div>

    <div class="section">
        <h2>3. Correlation Analysis</h2>
        <div class="visualization">
            <img src="data:image/png;base64,%s" alt="Correlation Heatmap">
            <p class="insight">Correlation heatmap showing relationships between different metrics.</p>
        </div>
    </div>

    <div class="section">
        <h2>4. Outlier Analysis</h2>
        <div class="visualization">
            <h3>Before Outlier Treatment</h3>
            <img src="data:image/png;base64,%s" alt="Outliers Before">
            <h3>After Outlier Treatment</h3>
            <img src="data:image/png;base64,%s" alt="Outliers After">
            <p class="insight">Comparison of data distribution before and after outlier treatment.</p>
        </div>
    </div>
</body>
</html>
', 
                          dim(dataset1)[1], dim(dataset1)[2],
                          dim(dataset2)[1], dim(dataset2)[2],
                          dim(merged_dataset)[1], dim(merged_dataset)[2],
                          plot_data$univariate,
                          plot_data$correlation,
                          plot_data$outliers_before,
                          plot_data$outliers_after
  )
  
  return(html_content)
}

##############################   Main Analysis Pipeline   ##############################

# 1. Data Preprocessing
dataset1 <- subset(dataset1, select = -`Sno`)
dataset2 <- dataset2[, -1]

# 2. Handle Missing Data
dataset1 <- handling_missing_data(dataset1)
dataset2 <- handling_missing_data(dataset2)

# 3. Merge Datasets
merged_dataset <- merge(dataset1, dataset2, by = "Date", all = TRUE)
merged_dataset <- handling_missing_data(merged_dataset)

# 4. Generate Plots
analysis_plots <- generate_analysis_plots(dataset1, dataset2, merged_dataset)

# 5. Generate and Save HTML Report
html_report <- generate_html_report(dataset1, dataset2, merged_dataset, analysis_plots)
writeLines(html_report, "covid19_analysis_report.html")

cat("Analysis complete. Report saved as 'covid19_analysis_report.html'")