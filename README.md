# 🦠 COVID-19 Deaths Prediction in India using R

Welcome to the **COVID-19 Deaths Prediction Project**, a comprehensive R-based system for analyzing and forecasting death rates due to the COVID-19 pandemic in India. Leveraging statistical methods and machine learning, this project primarily applies **Linear Regression** on real-world COVID-19 datasets to make predictions across different temporal windows.

![image](https://github.com/user-attachments/assets/8f66244d-7626-472d-b739-8069b0fc48dd)

---

## 🚀 Project Overview

This project offers:

1. **Data-Driven Modeling** 📊: Predicts COVID-19 death trends using cleaned and preprocessed datasets.
2. **Time-Based Forecasting** ⏱️: Explores short-term forecasting using sliding time windows.
3. **Statistical Rigor** 📐: Includes statistical tests such as **Shapiro-Wilk** and **Kruskal-Wallis** for performance validation.
4. **Visual Insights** 📈: Uses `ggplot2` to visualize data distribution, regression fit, and residuals.

---

## 🧠 Key Features

✅ End-to-end data preprocessing and cleaning pipeline
✅ Multiple modeling scenarios for weekly forecasting
✅ Visual and statistical evaluation of models
✅ Comparative analysis across different time windows
✅ Transparent code and reproducible methodology

---

## 🧬 Data Components & Workflow

### 1️⃣ Data Understanding

* Summary statistics and structural overview
* Missing value detection and handling
* Feature distribution and correlation heatmaps
* Outlier detection using boxplots and visual tools

### 2️⃣ Data Preparation

* **Missing Values**: Imputation using median/mode
* **Outliers**: IQR method for removal
* **Encoding**: Transformation of categorical variables
* **Feature Engineering**: Based on correlation analysis
* **Standardization**: Ensures consistent scaling
* **Statistical Testing**: Shapiro-Wilk and Kruskal-Wallis

### 3️⃣ Modeling Approach

Implements three forecasting strategies:

* 🧪 **Model A**: Weeks 1-4 → Predict Week 5
* 🧪 **Model B**: Weeks 1-4 → Predict Week 6
* 🧪 **Model C**: Week 4 → Predict Weeks 5 and 6

### 4️⃣ Evaluation Metrics

Models are validated using:

* **R-squared & Adjusted R-squared**
* **Standard Error of Estimate**
* **Scatterplots & Regression Lines**
* **Residual Plots**
* **Kruskal-Wallis Test** for statistical relevance

---

## 🛠️ Technical Stack

| Tool/Package | Purpose                       |
| ------------ | ----------------------------- |
| `readr`      | Data import and parsing       |
| `ggplot2`    | Visualization                 |
| `dplyr`      | Data manipulation             |
| `reshape2`   | Data reshaping                |
| `lubridate`  | Date parsing and manipulation |
| `gridExtra`  | Multi-plot layouts            |

Developed entirely in the **R Programming Environment**.

---

## 🗃️ Data Requirements

The project uses the following datasets:

1. **COVID-19 India Dataset** — `covid_19_india.csv`

   * Contains state-wise daily records of COVID-19 cases and deaths.

2. **Global Cases Dataset** — `COVID-19_Cases.csv`

   * General dataset for cross-validation and comparative trends.

---

## 📈 Performance Insights

| Model Scenario       | Accuracy | Insight                                   |
| -------------------- | -------- | ----------------------------------------- |
| Weeks 1-4 → Week 5   | High     | Excellent short-term forecasting accuracy |
| Weeks 1-4 → Week 6   | Medium   | Slight decline due to longer-term horizon |
| Week 4 → Weeks 5 & 6 | Variable | Captures near-future trends effectively   |

* Regional variation significantly affects results
* Models perform better on consistent, complete datasets

---

## 🔮 Future Enhancements

🔧 Integrate more advanced ML models (e.g., Random Forest, XGBoost)
🔧 Regional segmentation for tailored predictions
🔧 Incorporate mobility and policy data for added context
🔧 Real-time data streaming support
🔧 Enhanced dashboarding via `shiny` or `flexdashboard`

---

## 🧪 Scientific Methodology

The workflow adheres to a rigorous research-driven approach:

1. **Problem Framing** 🧩: Hypothesis and goals
2. **Data Acquisition** 📥: Collecting validated, real-world data
3. **Data Exploration** 🔍: Visual and statistical profiling
4. **Modeling** 🤖: Regression training and parameter tuning
5. **Validation** 📏: Statistical tests and residual checks
6. **Interpretation** 📊: Actionable insights from model outputs

---

## ⚠️ Limitations & Considerations

  * Data incompleteness or lag impacts prediction fidelity
  * External events (lockdowns, policy shifts) not captured
  * Model accuracy diminishes with long-term horizons
  * Regional imbalance in data can skew global performance

---

## 📬 Contact

Created by **Dhouha Meliane**
📧 [dhouhameliane@esprit.tn](mailto:dhouhameliane@esprit.tn)
🔗 [LinkedIn](https://www.linkedin.com/in/your-link)

---
