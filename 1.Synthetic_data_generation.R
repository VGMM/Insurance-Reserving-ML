# Step 1: Set up the environment
library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(cowplot)
library(kableExtra)

# Step 2: Create Policy Dataset

# a. Generate policy dates
dt_policydates <- data.table(date_UW = seq(as.Date("2016/1/1"), as.Date("2017/12/31"), by = "day"))

# Generate policy counts
dt_policydates[, policycount := rpois(.N, 700)]

# Calculate lapse date and exposure days
dt_policydates[, ':='(
  date_lapse = date_UW %m+% years(1),
  expodays = as.integer(date_UW %m+% years(1) - date_UW)
)]

# Create policy prefix
dt_policydates[, pol_prefix := year(date_UW)*10000 + month(date_UW)*100 + mday(date_UW)]

# b. Add policy coverage types
dt_policydates[, ':='(
  Cover_B = round(policycount * 0.25),
  Cover_BO = round(policycount * 0.45)
)]
dt_policydates[, Cover_BOT := policycount - Cover_B - Cover_BO]

# Verify the results
print(head(dt_policydates))

# c. Create policy transaction file
dt_policy <- dt_policydates[rep(1:.N, policycount)]
dt_policy[, pol_seq := seq_len(.N), by = .(date_UW)]
dt_policy[, pol_number := as.character(pol_prefix * 10000 + pol_seq)]

# Assign coverage types
dt_policy[, Cover := 'BO']
dt_policy[, cum_cover := cumsum(Cover_B + Cover_BOT), by = date_UW]
dt_policy[pol_seq <= cum_cover, Cover := 'BOT']
dt_policy[, cum_cover := cumsum(Cover_B), by = date_UW]
dt_policy[pol_seq <= cum_cover, Cover := 'B']
dt_policy[, cum_cover := NULL]

# d. Add more policy details
dt_policy[, Brand := rep(rep(c(1,2,3,4), c(9,6,3,2)), length.out = .N)]
dt_policy[, Base_Price := rep(rep(c(600,550,300,150), c(9,6,3,2)), length.out = .N)]

# Add Model and Model multiplier
for (eachBrand in unique(dt_policy$Brand)) {
  dt_policy[Brand == eachBrand, ':='(
    Model = rep(rep(c(3,2,1,0), c(10, 7, 2, 1)), length.out = .N),
    Model_mult = rep(rep(c(1.15^3, 1.15^2, 1.15^1, 1), c(10, 7, 2, 1)), length.out = .N)
  )]
}

# Calculate final price
dt_policy[, Price := ceiling(Base_Price * Model_mult)]

# e. Finalize policy dataset
cols_policy <- c("pol_number", "date_UW", "date_lapse", "Cover", "Brand", "Model", "Price")
dt_policy <- dt_policy[, ..cols_policy]

# Display the first few rows of the policy dataset
kable(head(dt_policy), "html") %>%
  kable_styling("striped") %>%
  scroll_box(width = "100%")

# Visualize policy count by date
ggplot(dt_policydates, aes(x = date_UW, y = policycount)) +
  geom_line() +
  labs(title = "Policy Count by Date", x = "Underwriting Date", y = "Policy Count") +
  theme_minimal()


# Assuming dt_policy is already created from previous steps

library(data.table)

# Function to sample policies and generate claims
generate_claims <- function(dt_policy, coverage, sample_rate, severity_alpha, severity_beta) {
  policies <- dt_policy[Cover %like% coverage, which = TRUE]
  n_claims <- floor(length(policies) * sample_rate)
  sampled_policies <- sample(policies, n_claims)
  
  data.table(
    pol_number = dt_policy$pol_number[sampled_policies],
    claim_type = coverage,
    claim_sev = rbeta(n_claims, severity_alpha, severity_beta)
  )
}

# a. Generate Breakage claims
dt_claim_B <- generate_claims(dt_policy, "B", 0.15, 2, 5)

# b. Generate Oxidation claims
dt_claim_O <- generate_claims(dt_policy, "[BO]", 0.05, 5, 3)

# c. Generate Theft claims
dt_claim_T <- rbindlist(lapply(0:3, function(model) {
  policies <- dt_policy[Cover == "BOT" & Model == model, which = TRUE]
  sample_rate <- 0.05 * (model + 1)
  n_claims <- floor(length(policies) * sample_rate)
  sampled_policies <- sample(policies, n_claims)
  
  data.table(
    pol_number = dt_policy$pol_number[sampled_policies],
    claim_type = "T",
    claim_sev = rbeta(n_claims, 5, 0.5)
  )
}))

# d. Combine all claims and add policy details
dt_claim <- rbindlist(list(dt_claim_B, dt_claim_O, dt_claim_T))
dt_claim[dt_policy, on = "pol_number", ':='(
  date_UW = i.date_UW,
  date_lapse = i.date_lapse,
  Price = i.Price,
  Brand = i.Brand,
  Model = i.Model
)]

# e. Generate claim dates
dt_claim[, ':='(
  date_occur = date_UW + runif(.N) * as.numeric(date_lapse - date_UW),
  delay_report = rbeta(.N, 0.4, 10) * 365,
  delay_pay = 10 + rbeta(.N, 7, 7) * 40
)]

# Calculate date_report in a separate step
dt_claim[, date_report := date_occur + delay_report]

# Now calculate date_pay
dt_claim[, date_pay := date_report + delay_pay]

# Convert date columns to Date class
dt_claim[, ':='(
  date_occur = as.Date(date_occur, origin = "1970-01-01"),
  date_report = as.Date(date_report, origin = "1970-01-01"),
  date_pay = as.Date(date_pay, origin = "1970-01-01")
)]

# Remove temporary columns
dt_claim[, ':='(delay_report = NULL, delay_pay = NULL)]

# Verify the results
print(head(dt_claim))

# f. Calculate claim costs
dt_claim[, claim_cost := round(Price * claim_sev)]

# g. Assign unique claim numbers
dt_claim[, clm_prefix := year(date_occur)*10000 + month(date_occur)*100 + mday(date_occur)]
dt_claim[, clm_seq := seq_len(.N), by = clm_prefix]
dt_claim[, clm_number := as.character(clm_prefix * 10000 + clm_seq)]

# h. Implement competing hazards model
setorder(dt_claim, pol_number, date_occur)
dt_claim <- dt_claim[!duplicated(pol_number)]

# i. Finalize claim dataset
cols_claim <- c("clm_number", "pol_number", "claim_type", "claim_sev",
                "date_occur", "date_report", "date_pay", "claim_cost")
dt_claim <- dt_claim[, ..cols_claim]

# Display the first few rows of the claim dataset
kable(head(dt_claim), "html") %>%
  kable_styling("striped") %>%
  scroll_box(width = "100%")

# Save the dataset (optional)
# save(dt_claim, file = "dt_claim.rda")

# Visualizations for validation
library(ggplot2)

# Histogram of reporting delay
ggplot(dt_claim, aes(x = as.numeric(date_report - date_occur))) +
  geom_histogram(binwidth = 10) +
  labs(title = "Histogram of Reporting Delay", x = "Delay (days)", y = "Count") +
  theme_minimal()

# Histogram of payment delay
ggplot(dt_claim, aes(x = as.numeric(date_pay - date_report))) +
  geom_histogram(binwidth = 2) +
  labs(title = "Histogram of Payment Delay", x = "Delay (days)", y = "Count") +
  theme_minimal()



# First, let's check the structure of dt_claim
print(str(dt_claim))

# Check the names of columns in dt_claim
print(names(dt_claim))

# If 'Brand' is not in dt_claim, let's add it from dt_policy
if(!"Brand" %in% names(dt_claim)) {
  dt_claim <- merge(dt_claim, dt_policy[, .(pol_number, Brand)], by = "pol_number", all.x = TRUE)
}

# Now let's try to calculate the claim rate again
claim_rate <- dt_claim[, .(claim_count = .N), by = Brand]
policy_count <- dt_policy[, .(policy_count = .N), by = Brand]

# Merge claim count and policy count
claim_rate <- merge(claim_rate, policy_count, by = "Brand", all = TRUE)

# Calculate claim rate
claim_rate[, claim_rate := claim_count / policy_count]

# Display the claim rate data
print(claim_rate)

# Create the plot
library(ggplot2)
ggplot(claim_rate, aes(x = as.factor(Brand), y = claim_rate)) +
  geom_bar(stat = "identity") +
  labs(title = "Claim Rate by Phone Brand", x = "Brand", y = "Claim Rate") +
  theme_minimal()

# If the plot doesn't appear, try printing it explicitly
print(last_plot())


library(ggplot2)
library(data.table)

# a. Histogram of reporting delay
plot_reporting_delay <- ggplot(dt_claim, aes(x = as.numeric(date_report - date_occur))) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Reporting Delay",
       x = "Reporting Delay (days)",
       y = "Count") +
  theme_minimal()

# b. Histogram of payment delay
plot_payment_delay <- ggplot(dt_claim, aes(x = as.numeric(date_pay - date_report))) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Payment Delay",
       x = "Payment Delay (days)",
       y = "Count") +
  theme_minimal()

# c. Plot of policy count by date
plot_policy_count <- ggplot(dt_policydates, aes(x = date_UW, y = policycount)) +
  geom_line(color = "blue") +
  labs(title = "Policy Count by Date",
       x = "Underwriting Date",
       y = "Policy Count") +
  theme_minimal()

# d. Plot of claim rate by phone brand
claim_rate <- dt_claim[, .(claim_count = .N), by = Brand][
  dt_policy[, .(policy_count = .N), by = Brand],
  on = "Brand"
][, claim_rate := claim_count / policy_count]

plot_claim_rate <- ggplot(claim_rate, aes(x = as.factor(Brand), y = claim_rate)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(title = "Claim Rate by Phone Brand",
       x = "Brand",
       y = "Claim Rate") +
  theme_minimal()

# e. Other relevant summary statistics and visualizations

# Claim severity distribution by claim type
plot_claim_severity <- ggplot(dt_claim, aes(x = claim_sev, fill = claim_type)) +
  geom_density(alpha = 0.7) +
  labs(title = "Claim Severity Distribution by Claim Type",
       x = "Claim Severity",
       y = "Density") +
  theme_minimal()

# Claim count by claim type
claim_type_count <- dt_claim[, .N, by = claim_type]
plot_claim_type <- ggplot(claim_type_count, aes(x = claim_type, y = N)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Claim Count by Claim Type",
       x = "Claim Type",
       y = "Count") +
  theme_minimal()

# Summary statistics
summary_stats <- dt_claim[, .(
  mean_severity = mean(claim_sev),
  median_severity = median(claim_sev),
  mean_cost = mean(claim_cost),
  median_cost = median(claim_cost),
  mean_reporting_delay = mean(as.numeric(date_report - date_occur)),
  mean_payment_delay = mean(as.numeric(date_pay - date_report))
), by = claim_type]

print(summary_stats)

# Display all plots
library(gridExtra)
grid.arrange(plot_reporting_delay, plot_payment_delay, plot_policy_count, 
             plot_claim_rate, plot_claim_severity, plot_claim_type, 
             ncol = 2)




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

cumulative_totals_long <- data.table(
  date = as.Date(substr(names(cumulative_totals), 17, 24), format = "%Y%m%d"),
  cumulative_cost = as.numeric(cumulative_totals)
)
ggplot(cumulative_totals_long, aes(x = date, y = cumulative_cost)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Cumulative Claim Costs Over Time",
       x = "Date",
       y = "Cumulative Cost") +
  scale_y_continuous(labels = scales::comma)

#Optional: Save the updated dataset
save(dt_polclaim, file = "dt_polclaim_timesliced.RData")



