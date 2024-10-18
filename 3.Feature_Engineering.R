# Load required libraries
library(data.table)
library(lubridate)

# Function to normalize numeric columns
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Function to prepare dataset
prepare_dataset <- function(dt, numeric_cols, factor_cols) {
  # Convert categorical variables to factors
  dt[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]
  
  # Normalize numeric features
  dt[, (paste0(numeric_cols, "_norm")) := lapply(.SD, normalize), .SDcols = numeric_cols]
  
  # Handle infinite values in price_per_year if it exists
  if ("price_per_year" %in% names(dt)) {
    dt[, price_per_year := pmin(price_per_year, quantile(price_per_year, 0.99, na.rm = TRUE))]
    dt[, price_per_year_norm := normalize(price_per_year)]
  }
  
  return(dt)
}

# Prepare RBNS dataset
rbns_numeric_cols <- c("Price", "days_until_valuation", "policy_age_at_report", "claim_amount", "time_to_payment")
rbns_factor_cols <- c("Cover", "Brand", "Model", "set")
dt_rbns <- prepare_dataset(dt_rbns, rbns_numeric_cols, rbns_factor_cols)

# Prepare IBNR Frequency dataset
ibnr_freq_numeric_cols <- c("Price", "exposure_years", "policy_age", "price_per_year")
ibnr_freq_factor_cols <- c("Cover", "Brand", "Model")
dt_ibnr_freq <- prepare_dataset(dt_ibnr_freq, ibnr_freq_numeric_cols, ibnr_freq_factor_cols)

# Add delay_uw to IBNR Frequency dataset
dt_ibnr_freq[, delay_uw := as.numeric(difftime(as.Date("2016-09-27"), date_UW, units = "days"))]
dt_ibnr_freq[, delay_uw_norm := normalize(delay_uw)]

# Prepare IBNR Severity dataset
ibnr_severity_numeric_cols <- c("Price", "exposure_years", "policy_age", "price_per_year", "claim_amount")
ibnr_severity_factor_cols <- c("Cover", "Brand", "Model")
dt_ibnr_severity <- prepare_dataset(dt_ibnr_severity, ibnr_severity_numeric_cols, ibnr_severity_factor_cols)

# Add delay_uw to IBNR Severity dataset
dt_ibnr_severity[, delay_uw := as.numeric(difftime(as.Date("2016-09-27"), date_UW, units = "days"))]
dt_ibnr_severity[, delay_uw_norm := normalize(delay_uw)]

# Verify the changes
print("RBNS Dataset Structure:")
print(str(dt_rbns))

print("IBNR Frequency Dataset Structure:")
print(str(dt_ibnr_freq))

print("IBNR Severity Dataset Structure:")
print(str(dt_ibnr_severity))

# Check for any remaining NA or infinite values
print("NA or Infinite values in RBNS dataset:")
print(sapply(dt_rbns, function(x) sum(is.na(x) | is.infinite(x))))

print("NA or Infinite values in IBNR Frequency dataset:")
print(sapply(dt_ibnr_freq, function(x) sum(is.na(x) | is.infinite(x))))

print("NA or Infinite values in IBNR Severity dataset:")
print(sapply(dt_ibnr_severity, function(x) sum(is.na(x) | is.infinite(x))))

# Optional: Save the prepared datasets
save(dt_rbns, dt_ibnr_freq, dt_ibnr_severity, file = "prepared_ml_datasets.RData")



# Function to print column types
print_column_types <- function(dt, name) {
  cat("\nColumn types for", name, ":\n")
  print(sapply(dt, class))
}

# Function to print summary statistics
print_summary_stats <- function(dt, name) {
  cat("\nSummary statistics for", name, ":\n")
  print(summary(dt))
}

# Run diagnostics
print_column_types(dt_rbns, "RBNS dataset")
print_column_types(dt_ibnr_freq, "IBNR Frequency dataset")
print_column_types(dt_ibnr_severity, "IBNR Severity dataset")

print_summary_stats(dt_rbns, "RBNS dataset")
print_summary_stats(dt_ibnr_freq, "IBNR Frequency dataset")
print_summary_stats(dt_ibnr_severity, "IBNR Severity dataset")
