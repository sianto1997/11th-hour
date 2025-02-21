# Load Data
cholesterol <- read.table("../ASSIGNMENT 1 FILES/cholesterol.txt", header=TRUE)

# Check Normality of Differences
differences <- cholesterol$After8weeks - cholesterol$Before
shapiro.test(differences)  # Shapiro-Wilk test for normality
hist(differences, main="Histogram of Differences", xlab="Cholesterol Difference")

# Paired t-test
t.test(cholesterol$Before, cholesterol$After8weeks, paired=TRUE)

# Wilcoxon Signed Rank Test (non-parametric)
wilcox.test(cholesterol$Before, cholesterol$After8weeks, paired=TRUE)

# Permutation Test
set.seed(123)
B <- 1000  # Number of permutations
perm_stats <- numeric(B)

for (i in 1:B) {
  perm_data <- ifelse(runif(length(differences)) > 0.5, differences, -differences)
  perm_stats[i] <- mean(perm_data)
}

observed_stat <- mean(differences)
p_value <- mean(abs(perm_stats) >= abs(observed_stat))

# Print Permutation Test Results
cat("Permutation Test p-value:", p_value, "\n")
