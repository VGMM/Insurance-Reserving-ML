# Insurance Reserving with Machine Learning

This project implements advanced machine learning techniques to enhance insurance reserving accuracy for RBNS (Reported But Not Settled) and IBNR (Incurred But Not Reported) claims.

## Project Overview

We developed a comprehensive reserving model using XGBoost to predict both the frequency and severity of insurance claims. The project aims to improve upon traditional actuarial methods by leveraging granular policy-level data and capturing complex, non-linear relationships.

### Key Features

- Synthetic data generation mimicking real-world insurance portfolios
- Separate modeling for RBNS and IBNR claims
- Feature engineering to capture time-based and policy-specific characteristics
- XGBoost implementation for both classification (claim occurrence) and regression (claim amount)
- Model interpretability using SHAP values and partial dependence plots

## Results

- RBNS Model: Achieved 98.23% accuracy in total reserve prediction
- IBNR Model: Implemented a two-stage approach combining frequency and severity predictions
- Enhanced granularity: Provided policy-level reserve estimates

## Repository Structure

- `1.Synthetic_data_generation.R`: Scripts for generating synthetic insurance data
- `2.EDA.R`: Exploratory Data Analysis of the generated datasets
- `3.Feature_Engineering.R`: Data cleaning and feature engineering
- `4.model_building.R`: RBNS and IBNR claims modeling using XGBoost

## Technologies Used

- R
- data.table for efficient data manipulation
- XGBoost for machine learning models
- ggplot2 for visualizations
- SHAPforxgboost for model interpretability

## Key Findings

- The RBNS model showed high accuracy in predicting total reserves
- The IBNR model demonstrated the ability to capture complex patterns in claim occurrence and severity
- Feature importance analysis revealed key drivers of claim costs and frequencies

## Future Work

- Refine IBNR predictions to address current underestimation
- Incorporate additional external factors for more robust predictions
- Develop a user-friendly interface for model deployment
