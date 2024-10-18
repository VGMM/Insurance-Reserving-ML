# Ensure required libraries are loaded
library(data.table)

# Step 1: Review existing datasets
print(str(dt_policy))
print(str(dt_claim))

# Step 2: Identify common keys
# The common key is 'pol_number'

# Step 3: Create joined dataset
dt_polclaim <- dt_policy[dt_claim, on = "pol_number", nomatch = 0]

# Step 4: Ensure all relevant features are included
# Add any missing features from dt_policy that aren't in dt_claim
policy_cols <- setdiff(names(dt_policy), names(dt_polclaim))
if (length(policy_cols) > 0) {
  dt_polclaim[dt_policy, on = "pol_number", (policy_cols) := mget(paste0("i.", policy_cols))]
}

# Step 5: Add a flag for policies with claims
dt_polclaim[, has_claim := TRUE]

# Step 6: Add policies without claims
dt_polclaim <- rbindlist(list(
  dt_polclaim,
  dt_policy[!pol_number %in% dt_polclaim$pol_number][, has_claim := FALSE]
), fill = TRUE)

# Step 7: Set keys for efficient processing
setkey(dt_polclaim, pol_number, date_UW)

# Step 8: Review the joined dataset
print(str(dt_polclaim))
print(head(dt_polclaim))
print(summary(dt_polclaim))

# Step 9: Check for any unexpected NULL or NA values
print(dt_polclaim[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = names(dt_polclaim)])

# Ensure required libraries are loaded
library(data.table)
library(lubridate)

# Step 1: Define time slices
min_date <- min(dt_polclaim$date_UW)
max_date <- max(dt_polclaim$date_pay, na.rm = TRUE)
time_slices <- seq(floor_date(min_date, "month"), ceiling_date(max_date, "month"), by = "30 days")

# Step 2: Create a function to calculate cumulative claim costs
calculate_cumulative_cost <- function(payment_date, cost, slice_date) {
  ifelse(payment_date <= slice_date, cost, 0)
}

# Step 3: Add time slice columns to the dataset
for (i in seq_along(time_slices)) {
  slice_date <- time_slices[i]
  col_name <- paste0("cumulative_cost_", format(slice_date, "%Y%m%d"))
  
  dt_polclaim[, (col_name) := calculate_cumulative_cost(date_pay, claim_cost, slice_date)]
}

# Step 4: Calculate total cumulative cost for each policy
cumulative_cols <- grep("cumulative_cost_", names(dt_polclaim), value = TRUE)
dt_polclaim[, total_cumulative_cost := rowSums(.SD, na.rm = TRUE), .SDcols = cumulative_cols]

# Step 5: Verify the results
print(head(dt_polclaim[has_claim == TRUE, c("pol_number", "date_pay", "claim_cost", ..cumulative_cols, "total_cumulative_cost")]))

# Step 6: Summarize time-sliced data
summary_stats <- dt_polclaim[, .(
  total_policies = .N,
  policies_with_claims = sum(has_claim),
  total_claim_cost = sum(claim_cost, na.rm = TRUE),
  max_cumulative_cost = max(total_cumulative_cost, na.rm = TRUE)
)]
print(summary_stats)

# Step 7: Plot cumulative claim costs over time
library(ggplot2)

cumulative_totals <- dt_polclaim[, lapply(.SD, sum, na.rm = TRUE), .SDcols = cumulative_cols]
library(data.table)
cumulative_totals_long <- melt(as.data.table(cumulative_totals), 
                               measure.vars = cumulative_cols,
                               variable.name = "date", 
                               value.name = "cumulative_cost")
setDT(cumulative_totals_long)  # This ensures cumulative_totals_long is a data.table

cumulative_totals_long[, date := as.Date(substr(as.character(date), 17, 24), format = "%Y%m%d")]

ggplot(cumulative_totals_long, aes(x = date, y = cumulative_cost)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Cumulative Claim Costs Over Time",
       x = "Date",
       y = "Cumulative Cost") +
  scale_y_continuous(labels = scales::comma)

#Optional: Save the updated dataset
save(dt_polclaim, file = "dt_polclaim_timesliced.RData")




# Ensure required libraries are loaded
library(data.table)
library(lubridate)

# Step 1: Define the valuation date (10th time slice)
time_slices <- as.Date(gsub("cumulative_cost_", "", grep("cumulative_cost_", names(dt_polclaim), value = TRUE)), format = "%Y%m%d")
valuation_date <- time_slices[10]

# Step 2: Create training data (claims reported prior to valuation date)
dt_rbns_train <- dt_polclaim[date_report <= valuation_date & date_pay > valuation_date]

# Step 3: Create test data (future claim payment periods for RBNS reserve calculation)
dt_rbns_test <- dt_polclaim[date_report <= valuation_date & date_pay > valuation_date]

# Step 4: Add features for RBNS analysis
add_rbns_features <- function(dt) {
  dt[, `:=`(
    days_until_valuation = as.numeric(valuation_date - date_report),
    policy_age_at_report = as.numeric(date_report - date_UW),
    claim_amount = claim_cost,
    time_to_payment = as.numeric(date_pay - valuation_date)
  )]
  return(dt)
}

dt_rbns_train <- add_rbns_features(dt_rbns_train)
dt_rbns_test <- add_rbns_features(dt_rbns_test)

# Step 5: Select relevant features for RBNS model
rbns_features <- c("pol_number", "Cover", "Brand", "Model", "Price", 
                   "days_until_valuation", "policy_age_at_report", 
                   "claim_amount", "time_to_payment")

dt_rbns_train <- dt_rbns_train[, ..rbns_features]
dt_rbns_test <- dt_rbns_test[, ..rbns_features]

# Step 6: Add a flag to distinguish between train and test sets
dt_rbns_train[, set := "train"]
dt_rbns_test[, set := "test"]

# Step 7: Combine train and test sets
dt_rbns <- rbindlist(list(dt_rbns_train, dt_rbns_test))

# Step 8: Verify the results
print(summary(dt_rbns))
print(table(dt_rbns$set))

# Step 9: Visualize the distribution of claim amounts in train and test sets
library(ggplot2)

ggplot(dt_rbns, aes(x = claim_amount, fill = set)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  theme_minimal() +
  labs(title = "Distribution of Claim Amounts in RBNS Train and Test Sets",
       x = "Claim Amount (log scale)",
       y = "Density")

# Optional: Save the RBNS dataset
save(dt_rbns, file = "dt_rbns.RData")

# Print the first few rows of the RBNS dataset
print(head(dt_rbns))


# Ensure required libraries are loaded
library(data.table)
library(lubridate)

# Step 1: Define the valuation date (using the same as in RBNS)
valuation_date <- time_slices[10]

# Step 2: Create base IBNR dataset (all policies with exposure prior to valuation date)
dt_ibnr_base <- dt_polclaim[date_UW <= valuation_date]

# Step 3: Add exposure information
dt_ibnr_base[, exposure_days := as.numeric(pmin(date_lapse, valuation_date) - date_UW)]
dt_ibnr_base[, exposure_years := exposure_days / 365.25]

# Verify the results
print(head(dt_ibnr_base[, .(pol_number, date_UW, date_lapse, exposure_days, exposure_years)]))
print(summary(dt_ibnr_base[, .(exposure_days, exposure_years)]))
# Step 4: Prepare data for Frequency model
dt_ibnr_freq <- dt_ibnr_base[, .(
  pol_number,
  Cover,
  Brand,
  Model,
  Price,
  date_UW,
  exposure_years,
  has_ibnr_claim = as.integer(date_occur > valuation_date | (is.na(date_occur) & has_claim == FALSE))
)]

# Step 5: Prepare data for Severity model
dt_ibnr_severity <- dt_ibnr_base[date_occur > valuation_date, .(
  pol_number,
  Cover,
  Brand,
  Model,
  Price,
  date_UW,
  exposure_years,
  claim_amount = claim_cost
)]

# Modify the add_ibnr_features function
add_ibnr_features <- function(dt) {
  dt[, policy_age := as.numeric(valuation_date - date_UW) / 365.25]
  dt[, price_per_year := Price / exposure_years]
  dt[, is_new_policy := as.integer(policy_age <= 0.25)]  # Consider policies less than 3 months old as new
  return(dt)
}

# Apply the corrected function
dt_ibnr_freq <- add_ibnr_features(dt_ibnr_freq)
dt_ibnr_severity <- add_ibnr_features(dt_ibnr_severity)

# Verify the results
print(head(dt_ibnr_freq))
print(head(dt_ibnr_severity))

# Check for any NA or infinite values
print(summary(dt_ibnr_freq))
print(summary(dt_ibnr_severity))

# Step 7: Split data into train and test sets (80% train, 20% test)
set.seed(123)  # for reproducibility
train_indices <- sample(1:nrow(dt_ibnr_freq), 0.8 * nrow(dt_ibnr_freq))

dt_ibnr_freq_train <- dt_ibnr_freq[train_indices]
dt_ibnr_freq_test <- dt_ibnr_freq[-train_indices]

dt_ibnr_severity_train <- dt_ibnr_severity[pol_number %in% dt_ibnr_freq_train$pol_number]
dt_ibnr_severity_test <- dt_ibnr_severity[pol_number %in% dt_ibnr_freq_test$pol_number]

# Step 8: Verify the results
print("Frequency Model Data:")
print(summary(dt_ibnr_freq_train))
print(table(dt_ibnr_freq_train$has_ibnr_claim))

print("Severity Model Data:")
print(summary(dt_ibnr_severity_train))

# Step 9: Visualize the data
library(ggplot2)

# Frequency model: IBNR claim occurrence by exposure years
ggplot(dt_ibnr_freq_train, aes(x = exposure_years, fill = factor(has_ibnr_claim))) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "IBNR Claim Occurrence by Exposure Years",
       x = "Exposure Years",
       y = "Density",
       fill = "Has IBNR Claim")

# Severity model: Claim amount by policy age
ggplot(dt_ibnr_severity_train, aes(x = policy_age, y = claim_amount)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "IBNR Claim Amount by Policy Age",
       x = "Policy Age (Years)",
       y = "Claim Amount")

# Optional: Save the IBNR datasets
save(dt_ibnr_freq_train, dt_ibnr_freq_test, dt_ibnr_severity_train, dt_ibnr_severity_test, file = "dt_ibnr.RData")



# Load required libraries
library(data.table)
library(lubridate)
library(ggplot2)

# Function to check date consistency
check_date_consistency <- function(dt, date_cols) {
  lapply(date_cols, function(col) {
    if (col %in% names(dt)) {
      print(paste("Summary of", col))
      print(summary(dt[[col]]))
      print(paste("NA count in", col, ":", sum(is.na(dt[[col]]))))
    } else {
      print(paste(col, "not found in the dataset"))
    }
  })
}

# Function to check numeric column distributions
check_numeric_distributions <- function(dt, numeric_cols) {
  lapply(numeric_cols, function(col) {
    if (col %in% names(dt)) {
      print(paste("Summary of", col))
      print(summary(dt[[col]]))
      ggplot(dt, aes(x = .data[[col]])) +
        geom_histogram(bins = 30) +
        labs(title = paste("Distribution of", col), x = col, y = "Count") +
        theme_minimal()
      print(last_plot())
    } else {
      print(paste(col, "not found in the dataset"))
    }
  })
}

# 1. Check original datasets
print("Checking original datasets:")
print(str(dt_policy))
print(str(dt_claim))

# 2. Check joined dataset (dt_polclaim)
print("Checking joined dataset (dt_polclaim):")
print(str(dt_polclaim))
print(summary(dt_polclaim))

# 3. Check time-sliced data
time_slice_cols <- grep("cumulative_cost_", names(dt_polclaim), value = TRUE)
print("Checking time-sliced data:")
print(summary(dt_polclaim[, ..time_slice_cols]))

# 4. Check RBNS dataset
print("Checking RBNS dataset:")
print(str(dt_rbns))
print(summary(dt_rbns))
print(table(dt_rbns$set))

# 5. Check IBNR datasets
print("Checking IBNR Frequency dataset:")
print(str(dt_ibnr_freq))
print(summary(dt_ibnr_freq))

print("Checking IBNR Severity dataset:")
print(str(dt_ibnr_severity))
print(summary(dt_ibnr_severity))

# 6. Check date consistency across datasets
date_cols <- c("date_UW", "date_lapse", "date_occur", "date_report", "date_pay")
check_date_consistency(dt_polclaim, date_cols)
check_date_consistency(dt_rbns, date_cols)
check_date_consistency(dt_ibnr_freq, date_cols)
check_date_consistency(dt_ibnr_severity, date_cols)

# 7. Check numeric distributions
numeric_cols <- c("Price", "claim_cost", "exposure_years", "policy_age", "price_per_year")
check_numeric_distributions(dt_polclaim, numeric_cols)
check_numeric_distributions(dt_rbns, numeric_cols)
check_numeric_distributions(dt_ibnr_freq, numeric_cols)
check_numeric_distributions(dt_ibnr_severity, numeric_cols)

# 8. Check categorical variables
cat_cols <- c("Cover", "Brand", "Model", "claim_type")
lapply(cat_cols, function(col) {
  if (col %in% names(dt_polclaim)) {
    print(paste("Distribution of", col))
    print(table(dt_polclaim[[col]], useNA = "ifany"))
  } else {
    print(paste(col, "not found in the dataset"))
  }
})

# 9. Check for any unexpected NULL or NA values
print("Checking for NULL or NA values in dt_polclaim:")
print(dt_polclaim[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = names(dt_polclaim)])

# 10. Verify alignment with Baudry's approach
print("Verifying alignment with Baudry's approach:")
print(paste("Number of time slices:", length(time_slice_cols)))
print(paste("RBNS valuation date:", valuation_date))
print(paste("IBNR datasets created:", exists("dt_ibnr_freq") && exists("dt_ibnr_severity")))

# 11. Check for data quality issues
print("Checking for data quality issues:")
print(paste("Negative claim costs:", sum(dt_polclaim$claim_cost < 0, na.rm = TRUE)))
print(paste("Future claim occurrences:", sum(dt_polclaim$date_occur > Sys.Date(), na.rm = TRUE)))
print(paste("Claims without policies:", sum(is.na(dt_claim$pol_number))))

# 12. Verify RBNS and IBNR split
print("Verifying RBNS and IBNR split:")
rbns_count <- nrow(dt_rbns)
ibnr_freq_count <- nrow(dt_ibnr_freq)
ibnr_sev_count <- nrow(dt_ibnr_severity)
total_policies <- nrow(dt_policy)

print(paste("RBNS claims:", rbns_count))
print(paste("IBNR frequency rows:", ibnr_freq_count))
print(paste("IBNR severity rows:", ibnr_sev_count))
print(paste("Total policies:", total_policies))
print(paste("Percentage of RBNS claims:", round(rbns_count / total_policies * 100, 2), "%"))
print(paste("Percentage of potential IBNR claims:", round(ibnr_freq_count / total_policies * 100, 2), "%"))


