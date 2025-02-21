# Load Data
cholesterol <- read.table("../ASSIGNMENT 1 FILES/cholesterol.txt", header=TRUE)

# Extract After8weeks column
after8weeks <- cholesterol$After8weeks
n <- length(after8weeks)

# 1. Normality-Based CI (using t-distribution)
mean_after <- mean(after8weeks)
sd_after <- sd(after8weeks)
alpha <- 0.03  # For 97% CI, alpha = 1 - 0.97

t_critical <- qt(1 - alpha/2, df = n-1)  # t-critical value for 97% CI
margin_of_error <- t_critical * (sd_after / sqrt(n))
CI_normal <- c(mean_after - margin_of_error, mean_after + margin_of_error)

cat("97% Confidence Interval based on Normality:", CI_normal, "\n")

# 2. Bootstrap 97% CI
set.seed(123)
B <- 10000  # Number of bootstrap samples
bootstrap_means <- numeric(B)

for (i in 1:B) {
  sample_bootstrap <- sample(after8weeks, size=n, replace=TRUE)
  bootstrap_means[i] <- mean(sample_bootstrap)
}

CI_bootstrap <- quantile(bootstrap_means, c(alpha/2, 1 - alpha/2))
cat("97% Bootstrap Confidence Interval:", CI_bootstrap, "\n")

# Compare Results
print(data.frame(Method=c("Normality-based CI", "Bootstrap CI"),
                 Lower=c(CI_normal[1], CI_bootstrap[1]),
                 Upper=c(CI_normal[2], CI_bootstrap[2])))