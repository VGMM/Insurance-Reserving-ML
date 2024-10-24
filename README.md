# Insurance Reserving with Machine Learning

An advanced machine learning approach to predict insurance reserves using XGBoost, achieving 98.23% accuracy in RBNS predictions and capturing complex claim patterns.

## Model Performance

![image](https://github.com/user-attachments/assets/e24f44a3-9694-4144-a468-8ff20c53d4fd)

*RBNS Reserves: High accuracy in predicting claim reserves across different occurrence months, with predicted values closely tracking actual reserves.*

## Key Insights

### Feature Impact Analysis
![image](https://github.com/user-attachments/assets/a5670395-78f1-4fd4-b4ab-91712ec54f8c)

Our XGBoost model revealed key drivers of claim costs:
- Policy price and age are the strongest predictors (4.9% and 4.4% impact)
- Time-based features (valuation delay, payment timing) show significant influence
- Brand and model characteristics provide additional predictive power

### Claim Patterns
![image](https://github.com/user-attachments/assets/16672322-9c01-4928-8db3-61ab89c59791)

Key findings from our analysis:
- Reporting delays follow an exponential distribution with most claims reported within 50 days
- Payment processing shows a normal distribution centered around 35 days
- Policy distribution remains stable across underwriting periods
- Distinct severity patterns observed between claim types (B vs BO claims)

## Technical Implementation

### Data Pipeline
1. **Synthetic Data Generation**
   - Realistic policy and claim simulation
   - Time-dependent feature engineering

2. **Model Architecture**
   - RBNS: Direct reserve prediction
   - IBNR: Two-stage frequency-severity approach
   - XGBoost implementation with SHAP interpretation
![image](https://github.com/user-attachments/assets/06f564ee-5705-4406-8c20-9a22b5e51d1d)

### Technologies Used
- R (data.table, ggplot2)
- XGBoost
- SHAP for interpretability

## Results and Impact

- **RBNS Accuracy:** 98.23% in reserve prediction
- **Feature Engineering:** Captured complex temporal patterns
- **Interpretability:** Clear insights into claim drivers
- **Granular Predictions:** Policy-level reserve estimates

## Repository Structure
