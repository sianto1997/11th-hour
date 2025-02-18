# Read the data
chol_data <- read.table("../ASSIGNMENT 1 FILES/cholesterol.txt", header = TRUE)
h = 300
w = h

# 1. Histograms and QQ plots
# First histogram
png("a/histogram_before.png", width = w, height = h)
hist(chol_data$Before, main = "Histogram of Cholesterol Before", 
     xlab = "Cholesterol (mmol/L)", probability = TRUE)
curve(dnorm(x, mean = mean(chol_data$Before), sd = sd(chol_data$Before)), 
      add = TRUE, col = "red")
dev.off()

# Second histogram
png("a/histogram_after.png", width = w, height = h)
hist(chol_data$After8weeks, main = "Histogram of Cholesterol After", 
     xlab = "Cholesterol (mmol/L)", probability = TRUE)
curve(dnorm(x, mean = mean(chol_data$After8weeks), sd = sd(chol_data$After8weeks)), 
      add = TRUE, col = "red")
dev.off()

# QQ plots
png("a/qqplot_before.png", width = w, height = h)
qqnorm(chol_data$Before, main = "Q-Q Plot Before")
qqline(chol_data$Before, col = "red")
dev.off()

png("a/qqplot_after.png", width = w, height = h)
qqnorm(chol_data$After8weeks, main = "Q-Q Plot After")
qqline(chol_data$After8weeks, col = "red")
dev.off()

# Scatterplot
png("a/correlation_plot.png", width = w, height = h)
plot(chol_data$Before, chol_data$After8weeks, 
     xlab = "Cholesterol Before (mmol/L)", 
     ylab = "Cholesterol After 8 weeks (mmol/L)",
     main = "Cholesterol: Before vs After 8 weeks")
abline(lm(After8weeks ~ Before, data = chol_data), col = "red")
dev.off()

# Statistical tests for normality
normality_before <- shapiro.test(chol_data$Before)
normality_after <- shapiro.test(chol_data$After8weeks)

# Correlation analysis
correlation_test <- cor.test(chol_data$Before, chol_data$After8weeks)

# Basic summary statistics
summary_stats <- summary(chol_data)

# Print results
cat("\nShapiro-Wilk Test Results:\n")
cat("Before:", "W =", normality_before$statistic, "p-value =", normality_before$p.value, "\n")
cat("After:", "W =", normality_after$statistic, "p-value =", normality_after$p.value, "\n")

cat("\nCorrelation Test Results:\n")
cat("Correlation coefficient:", correlation_test$estimate, "\n")
cat("p-value:", correlation_test$p.value, "\n")

cat("\nSummary Statistics:\n")
print(summary_stats)