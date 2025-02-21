# Load Data
cholesterol <- read.table("../ASSIGNMENT 1 FILES/cholesterol.txt", header=TRUE)

# Extract the "After8weeks" data
after8weeks <- cholesterol$After8weeks

# Set the significance level
alpha <- 0.05

# Define the range of theta values
theta_values <- seq(3, 12, by = 0.1)

# Function to perform the bootstrap test for a given theta
bootstrap_test <- function(data, theta, n_bootstrap = 1000) {
  observed_t <- max(data)
  bootstrap_t <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    bootstrap_sample <- runif(length(data), 3, theta)
    bootstrap_t[i] <- max(bootstrap_sample)
  }
  p_value <- mean(bootstrap_t >= observed_t)
  return(p_value)
}

# Perform the bootstrap test for each theta
p_values <- sapply(theta_values, function(theta) bootstrap_test(after8weeks, theta))

# Identify the theta values for which H0 is not rejected
non_rejected_theta <- theta_values[p_values >= alpha]

# Print the results
cat("Theta values for which H0 is not rejected (bootstrap):\n")
print(non_rejected_theta)


# Kolmogorov-Smirnov Test
ks_test_results <- sapply(theta_values, function(theta) {
  ks.test(after8weeks, "punif", min = 3, max = theta)$p.value
})

non_rejected_theta_ks <- theta_values[ks_test_results >= alpha]

cat("\nTheta values for which H0 is not rejected (KS test):\n")
print(non_rejected_theta_ks)

# Explanation for using KS test:
cat("\nExplanation for using the KS test:\n")
cat("The Kolmogorov-Smirnov test is applicable here because it is designed to test whether a sample comes from a specific distribution. In this case, we are testing whether the 'After8weeks' data follows a uniform distribution with parameters 3 and theta. The KS test compares the empirical cumulative distribution function (ECDF) of the sample to the theoretical CDF of the uniform distribution.  The test statistic is the maximum difference between these two CDFs.\n")
