# Load necessary libraries
library(data.table)
library(tidymodels)
library(xgboost)

# Inspect the 'Cover' variable
print(table(dt_rbns$Cover))

dt_rbns[, target := claim_amount_norm]

rbns_predictors <- c("Price", "Brand", "Model", "days_until_valuation", "policy_age_at_report", "time_to_payment")

print(paste("Levels of Brand:", paste(levels(dt_rbns$Brand), collapse = ", ")))
print(paste("Levels of Model:", paste(levels(dt_rbns$Model), collapse = ", ")))


# Check for any NA values in predictor variables
print(colSums(is.na(dt_rbns[, ..rbns_predictors])))

# Remove 'Cover' from predictors if it only has one level
if (length(unique(dt_rbns$Cover)) == 1) {
  rbns_predictors <- setdiff(rbns_predictors, "Cover")
}

# Convert 'Brand' and 'Model' to factors
dt_rbns[, `:=`(
  Brand = as.factor(Brand),
  Model = as.factor(Model)
)]

# Now let's try to prepare the data for xgboost again
rbns_recipe <- recipe(target ~ ., data = dt_rbns[, c(rbns_predictors, "target"), with = FALSE]) %>%
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  prep()

# Print a summary of the prepared data
summary(rbns_recipe)

# Split data into training and testing sets
set.seed(123)
rbns_split <- initial_split(dt_rbns, prop = 0.8)
rbns_train <- training(rbns_split)
rbns_test <- testing(rbns_split)

# Create xgboost matrices
rbns_train_data <- bake(rbns_recipe, new_data = rbns_train)
rbns_test_data <- bake(rbns_recipe, new_data = rbns_test)

# Print the first few rows and the structure of the prepared training data
print(head(rbns_train_data))
str(rbns_train_data)

# Print dimensions of the training and testing datasets
print(dim(rbns_train_data))
print(dim(rbns_test_data))

xgb_train <- xgb.DMatrix(data = as.matrix(select(rbns_train_data, -target)),
                         label = rbns_train_data$target)
xgb_test <- xgb.DMatrix(data = as.matrix(select(rbns_test_data, -target)),
                        label = rbns_test_data$target)

# Print dimensions of the training and testing matrices
print(dim(xgb_train))
print(dim(xgb_test))

# Set up XGBoost parameters
params <- list(
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Implement cross-validation
set.seed(123)
xgb_cv <- xgb.cv(
  params = params,
  data = xgb_train,
  nrounds = 1000,
  nfold = 5,
  early_stopping_rounds = 50,
  metrics = "rmse",
  verbose = 0
)

# Find optimal number of rounds
optimal_nrounds <- xgb_cv$best_iteration

# Train the initial model
xgb_model <- xgb.train(
  params = params,
  data = xgb_train,
  nrounds = optimal_nrounds,
  watchlist = list(train = xgb_train, test = xgb_test),
  verbose = 0
)

# Make predictions on test set
test_predictions <- predict(xgb_model, xgb_test)

# Calculate RMSE
rmse <- sqrt(mean((test_predictions - rbns_test_data$target)^2))
print(paste("RMSE:", rmse))

# Plot actual vs predicted values
plot_data <- data.frame(
  actual = rbns_test_data$target,
  predicted = test_predictions
)

ggplot(plot_data, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Values",
       x = "Actual",
       y = "Predicted") +
  theme_minimal()

# Generate feature importance plot
importance_matrix <- xgb.importance(feature_names = colnames(xgb_train), model = xgb_model)
xgb.plot.importance(importance_matrix, top_n = 10, measure = "Gain")


# Load necessary libraries
library(xgboost)
library(SHAPforxgboost)
library(ggplot2)
library(pdp)
library(lubridate)

# 1. Final Model Training
xgb_final <- xgb.train(
  params = params,
  data = xgb.DMatrix(data = as.matrix(select(rbns_train_data, -target)),
                     label = rbns_train_data$target),
  nrounds = optimal_nrounds
)

# 2. Model Inspection

# a. Generate feature importance plot
importance_matrix <- xgb.importance(feature_names = colnames(select(rbns_train_data, -target)), 
                                    model = xgb_final)
xgb.plot.importance(importance_matrix, top_n = 10, measure = "Gain")

# b. Create SHAP summary plot
shap_values <- shap.values(xgb_model = xgb_final, 
                           X_train = as.matrix(select(rbns_train_data, -target)))
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, 
                       X_train = as.matrix(select(rbns_train_data, -target)))
shap.plot.summary(shap_long)

# c. Produce partial dependence plots for top features
top_features <- importance_matrix$Feature[1:3]  # Top 3 features

pdp_plots <- lapply(top_features, function(feature) {
  partial_plot <- partial(xgb_final, pred.var = feature, 
                          train = as.matrix(select(rbns_train_data, -target)))
  plot(partial_plot, main = paste("Partial Dependence Plot for", feature))
})

# 3. Prediction and Evaluation

# a. Make predictions on test set
test_predictions <- predict(xgb_final, as.matrix(select(rbns_test_data, -target)))

# b. Summarize RBNS reserves by claim occurrence month
dt_rbns_test <- as.data.table(rbns_test_data)
dt_rbns_test[, claim_month := floor_date(as.Date(policy_age_at_report, origin = "1970-01-01"), "month")]
dt_rbns_test[, predicted_reserve := test_predictions]

rbns_summary <- dt_rbns_test[, .(
  actual_reserve = sum(target),
  predicted_reserve = sum(predicted_reserve)
), by = claim_month]

# c. Compare predictions to actual simulated claim run-off
ggplot(rbns_summary, aes(x = claim_month)) +
  geom_line(aes(y = actual_reserve, color = "Actual")) +
  geom_line(aes(y = predicted_reserve, color = "Predicted")) +
  labs(title = "RBNS Reserves: Actual vs Predicted",
       x = "Claim Occurrence Month",
       y = "Reserve Amount",
       color = "Legend") +
  theme_minimal()

# Calculate overall accuracy
total_actual <- sum(rbns_summary$actual_reserve)
total_predicted <- sum(rbns_summary$predicted_reserve)
accuracy <- (total_predicted - total_actual) / total_actual * 100

print(paste("Overall RBNS Reserve Prediction Accuracy:", round(accuracy, 2), "%"))

# Print summary statistics
print(rbns_summary)


library(data.table)
library(caret)
library(xgboost)


# Handle infinite values in 'price_per_year'
max_price <- max(dt_ibnr_freq_train$price_per_year[is.finite(dt_ibnr_freq_train$price_per_year)])
dt_ibnr_freq_train[, price_per_year := pmin(price_per_year, max_price)]
dt_ibnr_freq_test[, price_per_year := pmin(price_per_year, max_price)]

# Instead of creating dummy variables for 'pol_number', we'll use a numeric encoding
dt_ibnr_freq_train[, pol_number_encoded := as.numeric(factor(pol_number))]
dt_ibnr_freq_test[, pol_number_encoded := as.numeric(factor(pol_number, levels = levels(factor(dt_ibnr_freq_train$pol_number))))]

# Identify predictor columns
predictor_cols <- setdiff(names(dt_ibnr_freq_train), c("has_ibnr_claim", "pol_number", "date_UW"))

# Convert 'Cover' to factor
dt_ibnr_freq_train[, Cover := as.factor(Cover)]
dt_ibnr_freq_test[, Cover := as.factor(Cover)]

# Ensure all predictor columns are numeric
dt_ibnr_freq_train[, (predictor_cols) := lapply(.SD, as.numeric), .SDcols = predictor_cols]
dt_ibnr_freq_test[, (predictor_cols) := lapply(.SD, as.numeric), .SDcols = predictor_cols]

# Create XGBoost matrices
xgb_ibnr_freq_train <- xgb.DMatrix(data = as.matrix(dt_ibnr_freq_train[, ..predictor_cols]), 
                                   label = dt_ibnr_freq_train$has_ibnr_claim)
xgb_ibnr_freq_test <- xgb.DMatrix(data = as.matrix(dt_ibnr_freq_test[, ..predictor_cols]), 
                                  label = dt_ibnr_freq_test$has_ibnr_claim)

print("XGBoost matrices created successfully.")
print("Dimensions of xgb_ibnr_freq_train:")
print(dim(xgb_ibnr_freq_train))

# Set up XGBoost parameters
# Increase number of rounds and adjust some parameters
params <- list(
  objective = "binary:logistic",
  eta = 0.05,  # Reduced learning rate
  max_depth = 4,  # Reduced max depth to prevent overfitting
  subsample = 0.8,
  colsample_bytree = 0.8
)

set.seed(123)
xgb_cv <- xgb.cv(
  params = params,
  data = xgb_ibnr_freq_train,
  nrounds = 300,  # Increased from 100
  nfold = 3,
  early_stopping_rounds = 20,  # Increased from 10
  metrics = "auc",
  verbose = 0
)

optimal_nrounds <- xgb_cv$best_iteration
print(paste("Optimal number of rounds:", optimal_nrounds))
print(paste("Best AUC:", max(xgb_cv$evaluation_log$test_auc_mean)))

# Train the final model
final_model <- xgb.train(
  params = params,
  data = xgb_ibnr_freq_train,
  nrounds = optimal_nrounds
)

# Get feature importance
importance <- xgb.importance(feature_names = predictor_cols, model = final_model)
print(importance)

# Make predictions on test set
test_pred <- predict(final_model, xgb_ibnr_freq_test)

# Calculate AUC on test set
library(pROC)
test_auc <- auc(dt_ibnr_freq_test$has_ibnr_claim, test_pred)
print(paste("Test AUC:", test_auc))





print(str(dt_ibnr_severity))
print(summary(dt_ibnr_severity))

library(data.table)

# Convert character columns to factors
char_cols <- names(dt_ibnr_severity)[sapply(dt_ibnr_severity, is.character)]
dt_ibnr_severity[, (char_cols) := lapply(.SD, as.factor), .SDcols = char_cols]

# Handle infinite values in 'price_per_year' if they exist
max_price <- max(dt_ibnr_severity$price_per_year[is.finite(dt_ibnr_severity$price_per_year)])
dt_ibnr_severity[, price_per_year := pmin(price_per_year, max_price)]

# Encode pol_number
dt_ibnr_severity[, pol_number_encoded := as.numeric(factor(pol_number))]

# Identify predictor columns
severity_predictor_cols <- setdiff(names(dt_ibnr_severity), c("claim_amount", "pol_number", "date_UW"))

# Ensure all predictor columns are numeric
dt_ibnr_severity[, (severity_predictor_cols) := lapply(.SD, as.numeric), .SDcols = severity_predictor_cols]

# Split data into training and testing sets (80% train, 20% test)
set.seed(123)
train_indices <- sample(1:nrow(dt_ibnr_severity), 0.8 * nrow(dt_ibnr_severity))
dt_severity_train <- dt_ibnr_severity[train_indices]
dt_severity_test <- dt_ibnr_severity[-train_indices]

# Create XGBoost matrices
xgb_severity_train <- xgb.DMatrix(data = as.matrix(dt_severity_train[, ..severity_predictor_cols]), 
                                  label = dt_severity_train$claim_amount)
xgb_severity_test <- xgb.DMatrix(data = as.matrix(dt_severity_test[, ..severity_predictor_cols]), 
                                 label = dt_severity_test$claim_amount)

print("XGBoost matrices created successfully.")
print("Dimensions of xgb_severity_train:")
print(dim(xgb_severity_train))

# Now proceed with the rest of the code (cross-validation, model training, etc.)

# Set up XGBoost parameters
params <- list(
  objective = "reg:squarederror",  # for regression
  eta = 0.05,
  max_depth = 4,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Perform cross-validation
set.seed(123)
xgb_cv <- xgb.cv(
  params = params,
  data = xgb_severity_train,
  nrounds = 300,
  nfold = 3,
  early_stopping_rounds = 20,
  metrics = "rmse",  # Root Mean Squared Error for regression
  verbose = 0
)

# Find optimal number of rounds
optimal_nrounds <- xgb_cv$best_iteration
print(paste("Optimal number of rounds:", optimal_nrounds))
print(paste("Best RMSE:", min(xgb_cv$evaluation_log$test_rmse_mean)))

# Train the final model
final_model <- xgb.train(
  params = params,
  data = xgb_severity_train,
  nrounds = optimal_nrounds
)

# Get feature importance
importance <- xgb.importance(feature_names = severity_predictor_cols, model = final_model)
print(importance)

# Continue with the rest of the code (SHAP plots, predictions, evaluation)


library(data.table)
library(xgboost)

# Prepare data for severity model
severity_predictor_cols <- setdiff(names(dt_ibnr_severity_train), c("claim_amount", "pol_number", "date_UW"))

# Convert categorical variables to factors and ensure all predictors are numeric
dt_ibnr_severity_train[, (severity_predictor_cols) := lapply(.SD, function(x) {
  if(is.character(x)) as.factor(x) else x
}), .SDcols = severity_predictor_cols]
dt_ibnr_severity_test[, (severity_predictor_cols) := lapply(.SD, function(x) {
  if(is.character(x)) as.factor(x) else x
}), .SDcols = severity_predictor_cols]

dt_ibnr_severity_train[, (severity_predictor_cols) := lapply(.SD, as.numeric), .SDcols = severity_predictor_cols]
dt_ibnr_severity_test[, (severity_predictor_cols) := lapply(.SD, as.numeric), .SDcols = severity_predictor_cols]

# Handle infinite or NaN values in severity predictors
for (col in severity_predictor_cols) {
  dt_ibnr_severity_train[is.infinite(get(col)) | is.nan(get(col)), (col) := NA]
  dt_ibnr_severity_test[is.infinite(get(col)) | is.nan(get(col)), (col) := NA]
}

# Remove rows with NA values
dt_ibnr_severity_train <- na.omit(dt_ibnr_severity_train)
dt_ibnr_severity_test <- na.omit(dt_ibnr_severity_test)

# Create XGBoost matrices for severity model
xgb_severity_train <- xgb.DMatrix(data = as.matrix(dt_ibnr_severity_train[, ..severity_predictor_cols]), 
                                  label = dt_ibnr_severity_train$claim_amount)
xgb_severity_test <- xgb.DMatrix(data = as.matrix(dt_ibnr_severity_test[, ..severity_predictor_cols]), 
                                 label = dt_ibnr_severity_test$claim_amount)

# Set up parameters for severity model
params_severity <- list(
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Train severity model
set.seed(123)
xgb_cv_severity <- xgb.cv(
  params = params_severity,
  data = xgb_severity_train,
  nrounds = 1000,
  nfold = 5,
  early_stopping_rounds = 50,
  metrics = "rmse",
  verbose = 0
)

optimal_nrounds_severity <- xgb_cv_severity$best_iteration

final_severity_model <- xgb.train(
  params = params_severity,
  data = xgb_severity_train,
  nrounds = optimal_nrounds_severity
)

# Severity predictions
sev_pred <- predict(final_severity_model, xgb_severity_test)
dt_ibnr_severity_test[, pred_severity := NA_real_]
dt_ibnr_severity_test[1:length(sev_pred), pred_severity := sev_pred]



# Add predictions to dt_ibnr_freq_test
dt_ibnr_freq_test[, pred_frequency := test_pred]

# Combine frequency and severity predictions
dt_ibnr_combined <- merge(dt_ibnr_freq_test, dt_ibnr_severity_test, by = "pol_number", all.x = TRUE)
dt_ibnr_combined[, pred_ibnr_reserve := pred_frequency * pred_severity]

print(names(dt_ibnr_freq_test))
print(names(dt_ibnr_severity_test))
print(names(dt_ibnr_combined))

summary_stats <- dt_ibnr_combined[, .(
  avg_pred_frequency = mean(pred_frequency, na.rm = TRUE),
  avg_pred_severity = mean(pred_severity, na.rm = TRUE),
  avg_pred_reserve = mean(pred_ibnr_reserve, na.rm = TRUE),
  total_pred_reserve = sum(pred_ibnr_reserve, na.rm = TRUE)
)]
print(summary_stats)

# Visualize predicted vs actual IBNR reserves
library(ggplot2)
plot <- ggplot(dt_ibnr_combined, aes(x = has_ibnr_claim * claim_amount, y = pred_ibnr_reserve)) +
  geom_point(alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual IBNR Reserves",
       x = "Actual IBNR Reserve",
       y = "Predicted IBNR Reserve") +
  theme_minimal()
print(plot)



# Aggregate by underwriting period (assuming date_UW.x is the underwriting date)
dt_ibnr_combined[, underwriting_period := format(date_UW.x, "%Y-%m")]

ibnr_by_period <- dt_ibnr_combined[, .(
  actual_ibnr = sum(has_ibnr_claim * claim_amount, na.rm = TRUE),
  predicted_ibnr = sum(pred_ibnr_reserve, na.rm = TRUE)
), by = underwriting_period]

# Calculate total actual and predicted IBNR
total_actual_ibnr <- sum(ibnr_by_period$actual_ibnr)
total_predicted_ibnr <- sum(ibnr_by_period$predicted_ibnr)

# Calculate prediction error
prediction_error <- (total_predicted_ibnr - total_actual_ibnr) / total_actual_ibnr * 100

print(paste("Total Actual IBNR:", total_actual_ibnr))
print(paste("Total Predicted IBNR:", total_predicted_ibnr))
print(paste("Prediction Error:", round(prediction_error, 2), "%"))

# Create a development triangle
library(reshape2)

ibnr_triangle <- dcast(ibnr_by_period, underwriting_period ~ ., value.var = "predicted_ibnr")
print(ibnr_triangle)

# Visualize predicted vs actual IBNR by underwriting period
ggplot(ibnr_by_period, aes(x = actual_ibnr, y = predicted_ibnr)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual IBNR Reserves by Underwriting Period",
       x = "Actual IBNR Reserve",
       y = "Predicted IBNR Reserve") +
  theme_minimal()

