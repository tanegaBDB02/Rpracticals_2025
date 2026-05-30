# ===== Question 1 =====
# (a) For α = 22.5, b = 51.6, and c = 11.4, compute Q according to:
#     Q = (a^T + b^T + c^T)/(a + b + c) * exp(-b/(a + c))
#     Print Q
# (3 marks)
a <- 22.5
b <- 51.6
c <- 11.4
Q <- (t(a) + t(b) + t(c)) / (a + b + c) * exp(-b / (a + c))
print(Q)

# (b) Create vectors X and Y:
#     X = c(12, 34, 9, 32, 24, 43, 52, 16, 2, 17, 39, 57, 72, 82, 92, 4, 59, 79, 99)
#     Y = c(33, 24, 15, 49, 22, 48, 66, 10, 4, 24, 42, 50, 66, 75, 99, 9, 61, 72, 90)
#     1. Create "Xsmall" (elements > 50 in X) and print
#     2. Create "Ywindow" (elements > 20 & < 70 in Y) and print
# (4 marks)
X <- c(12, 34, 9, 32, 24, 43, 52, 16, 2, 17, 39, 57, 72, 82, 92, 4, 59, 79, 99)
Y <- c(33, 24, 15, 49, 22, 48, 66, 10, 4, 24, 42, 50, 66, 75, 99, 9, 61, 72, 90)

Xsmall <- X[X > 50]
print(Xsmall)

Ywindow <- Y[Y > 20 & Y < 70]
print(Ywindow)
# R-script name: Question-1.r


# ===== Question 2 =====
# (a) Read "sample data.csv" into data frame 'dat' and print
# (2 marks)
dat <- read.csv("sample data.csv")
print(dat)

# (b) Create subset 'subdat' by removing values > 800 from 'standard sample' column
#     Print subdat
# (2 marks)
subdat <- dat[dat$standard.sample <= 800, ]
print(subdat)

# (c) Divide canvas into 2 portions and plot:
#     1. Histogram of 'standard sample' (blue)
#     2. Histogram of 'new.sample' (red)
# (3 marks)
par(mfrow = c(1, 2))
hist(dat$standard.sample, col = "blue", main = "Standard Sample", xlab = "Value")
hist(dat$new.sample, col = "red", main = "New Sample", xlab = "Value")
# R-script name: Question-2.r


# ===== Question 3 =====
# (a) Generate 9000 random deviates from N(μ=16, σ=3)
# (1 mark)
data <- rnorm(9000, mean = 16, sd = 3)

# (b) Compute and print:
#     - Minimum
#     - Median
#     - Mean
#     - Variance
#     - Maximum
# (2 marks)
cat("Min:", min(data), "\n")
cat("Median:", median(data), "\n")
cat("Mean:", mean(data), "\n")
cat("Variance:", var(data), "\n")
cat("Max:", max(data), "\n")

# (c) Split data into two equal subsets '01' and '02'
#     Plot scatter plot of 01 vs 02
# (4 marks)
split <- split(data, rep(1:2, each = 4500))
o1 <- split[[1]]
o2 <- split[[2]]

plot(o1, o2, xlab = "Subset 1", ylab = "Subset 2", main = "Scatter Plot")
# R-script name: Question-3.r


# ===== Question 4 =====
#drugA <- c(3.5, 5.7, 3.4, 6.9, 17.8, 3.8, 3.0, 6.4, 6.8, 3.6, 6.9, 5.7)
#drugB <- c(4.5, 11.7, 10.8, 4.5, 6.3, 3.8, 6.2, 6.6, 7.1, 6.4, 4.5, 5.1, 3.2, 4.7, 4.5, 3.0)

# (a) Perform independent two-sample t-test (α = 0.05)
#     Print:
#     - t-statistic
#     - p-value
#     - Conclusion (reject/accept H0)
# (4 marks)
# Data
drugA <- c(3.5, 5.7, 3.4, 6.9, 17.8, 3.8, 3.0, 6.4, 6.8, 3.6, 6.9, 5.7)
drugB <- c(4.5, 11.7, 10.8, 4.5, 6.3, 3.8, 6.2, 6.6, 7.1, 6.4, 4.5, 5.1, 3.2, 4.7, 4.5, 3.0)

ttest_result <- t.test(drugA, drugB, var.equal = FALSE) 

t_statistic <- ttest_result$statistic
p_value <- ttest_result$p.value

cat("t-statistic:", round(t_statistic, 3), "\n")
cat("p-value:", round(p_value, 5), "\n")

if (p_value < 0.05) {
  cat("Conclusion: Reject the null hypothesis (H0)\n")
} else {
  cat("Conclusion: Fail to reject the null hypothesis (H0)\n")
}


# (b) Compute 95% CI for difference in means and print
# (3 marks)
cat("95% Confidence Interval for mean difference:\n")
print(ttest$conf.int)

# R-script name: Question-4.r


# ===== Question 5 =====
# strain1 <- c(11, 10, 9, 12, 13, 11)
# strain2 <- c(7, 8, 9, 7, 6, 6)
# strain3 <- c(9, 8, 10, 11, 10, 10)
# strain4 <- c(12, 11, 13, 14, 12, 13)

# Perform one-way ANOVA (α = 0.04)
# Print:
# - Null hypothesis statement
# - F-statistic
# - p-value
# - Conclusion
# (7 marks)
# Question 5
# Data
strain1 <- c(11, 10, 9, 12, 13, 11)
strain2 <- c(7, 8, 9, 7, 6, 6)
strain3 <- c(9, 8, 10, 11, 10, 10)
strain4 <- c(12, 11, 13, 14, 12, 13)


values <- c(strain1, strain2, strain3, strain4)
group <- factor(rep(c("strain1", "strain2", "strain3", "strain4"), each = 6))

anova_result <- aov(values ~ group)

summary_result <- summary(anova_result)
print(summary_result)


# R-script name: Question-5.r

# ===== Question 6 =====
# conc <- c(0.0, 50.0, 75.0, 100.0, 150.0, 200.0)
# signal <- c(1.72, 2.11, 2.36, 2.56, 3.05, 3.42)

# (a) Calculate least-square regression line (conc ~ signal)
#     Print:
#     - Slope
#     - Intercept
concentration <- c(0.0, 50.0, 75.0, 100.0, 150.0, 200.0)
signal <- c(1.72, 2.11, 2.36, 2.56, 3.05, 3.42)

model <- lm(signal ~ concentration)
cat("Slope:", coef(model)[2], "\n")
cat("Intercept:", coef(model)[1], "\n")

# (b) Plot original data and regression line
# (7 marks)
plot(concentration, signal, pch = 19, col = "blue", main = "Regression Line")
abline(model, col = "red", lwd = 2)

# R-script name: Question-6.r

