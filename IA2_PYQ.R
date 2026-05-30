# ===== Question 1 =====
# Cardiac output data
#cardiac_output <- c(4.91, 4.10, 6.74, 7.27, 7.42, 7.50, 6.56, 4.64, 5.98, 3.14, 3.23, 5.80, 6.17, 5.39, 5.77)

# (a) Perform t-test to check if population mean differs from 5.2
#     Print:
#     - Test statistic
#     - p-value
#     - Conclusion
# (5 marks)
cardiac_output <- c(4.91, 4.10, 6.74, 7.27, 7.42, 7.50, 6.56, 4.64, 5.98, 3.14, 3.23, 5.80, 6.17, 5.39, 5.77)

t_result <- t.test(cardiac_output, mu = 5.2)

cat("T-statistic:", t_result$statistic, "\n")
cat("P-value:", t_result$p.value, "\n")

if (t_result$p.value < 0.05) {
  cat("Conclusion: Reject the null hypothesis. Mean differs significantly from 5.2.\n")
} else {
  cat("Conclusion: Fail to reject the null hypothesis. No significant difference from 5.2.\n")
}

# (b) Compute and print 90% confidence interval for the data
# (5 marks)
ci_result <- t.test(cardiac_output, conf.level = 0.90)
cat("90% Confidence Interval:\n")
print(ci_result$conf.int)

# R-script name: Question-1.r


# ===== Question 2 =====
# drugA <- c(3.5, 5.7, 3.4, 6.9, 17.8, 3.8, 3.0, 6.4, 6.8, 3.6, 6.9, 5.7)
# drugB <- c(4.5, 11.7, 10.8, 4.5, 6.3, 3.8, 6.2, 6.6, 7.1, 6.4, 4.5, 5.1, 3.2, 4.7, 4.5, 3.0)

# (a) Perform two-sample Student's t-test (equal variances, α=0.05)
#     Print:
#     - t-statistic
#     - p-value
#     - Conclusion
# (5 marks)
drugA <- c(3.5, 5.7, 3.4, 6.9, 17.8, 3.8, 3.0, 6.4, 6.8, 3.6, 6.9, 5.7)
drugB <- c(4.5, 11.7, 10.8, 4.5, 6.3, 3.8, 6.2, 6.6, 7.1, 6.4, 4.5, 5.1, 3.2, 4.7, 4.5, 3.0)

t_test_equal <- t.test(drugA, drugB, var.equal = TRUE)

cat("Student's t-test:\n")
cat("T-statistic:", t_test_equal$statistic, "\n")
cat("P-value:", t_test_equal$p.value, "\n")

if (t_test_equal$p.value < 0.05) {
  cat("Conclusion: Reject the null hypothesis. Significant difference in means.\n")
} else {
  cat("Conclusion: Fail to reject the null hypothesis. No significant difference.\n")
}

# (b) Perform Welch's t-test (unequal variances)
#     Print results and compare with part (a)
# (5 marks)
t_test_unequal <- t.test(drugA, drugB)

cat("\nWelch's t-test:\n")
cat("T-statistic:", t_test_unequal$statistic, "\n")
cat("P-value:", t_test_unequal$p.value, "\n")

cat("\nComparison:\n")
cat("The p-values differ slightly due to the different assumptions about variance.\n")


# R-script name: Question-2.r


# ===== Question 3 =====
# chirps <- c(14.7, 19.8, 18.4, 17.1, 15.5, 19.5)
# temp <- c(69.7, 93.3, 84.3, 80.6, 75.2, 90.0)

# Fit least squares regression (temp ~ chirps)
# Print slope and intercept as plot legends
# Plot the data and regression line
# (10 marks)
chirps <- c(14.7, 19.8, 18.4, 17.1, 15.5, 19.5)
temp <- c(69.7, 93.3, 84.3, 80.6, 75.2, 90.0)

model <- lm(temp ~ chirps)

# Get slope and intercept
slope <- coef(model)[2]
intercept <- coef(model)[1]

# Plot with regression line and legend
plot(chirps, temp, pch = 16, col = "blue", main = "Regression: Temp ~ Chirps",
     xlab = "Chirps", ylab = "Temperature")
abline(model, col = "red", lwd = 2)

legend("topleft", legend = c(paste("Slope:", round(slope, 2)),
                             paste("Intercept:", round(intercept, 2))),
       bty = "n")


# R-script name: Question-3.r


# ===== Question 4 =====
# strain1 <- c(11, 10, 9, 12, 13, 11)
# strain2 <- c(7, 8, 9, 7, 6, 6)
# strain3 <- c(5, 10, 11, 13, 10)  
# strain4 <- c(12, 11, 13, 14, 12, 13) 

# (a) Perform one-way ANOVA (α=0.04)
#     Print:
#     (i) Null hypothesis statement
#     (ii) F-statistic value
#     (iii) p-value
#     (iv) Conclusion
# (7 marks)
strain1 <- c(11, 10, 9, 12, 13, 11)
strain2 <- c(7, 8, 9, 7, 6, 6)
strain3 <- c(5, 10, 11, 13, 10)
strain4 <- c(12, 11, 13, 14, 12, 13)

survival <- c(strain1, strain2, strain3, strain4)
group <- factor(rep(c("Strain1", "Strain2", "Strain3", "Strain4"),
                    times = c(length(strain1), length(strain2), length(strain3), length(strain4))))

anova_result <- aov(survival ~ group)

cat("(i) Null Hypothesis: All group means are equal.\n")
cat("(ii) F-statistic:", summary(anova_result)[[1]]$`F value`[1], "\n")
cat("(iii) P-value:", summary(anova_result)[[1]]$`Pr(>F)`[1], "\n")

if (summary(anova_result)[[1]]$`Pr(>F)`[1] < 0.04) {
  cat("(iv) Conclusion: Reject null hypothesis. Significant difference among strains.\n")
} else {
  cat("(iv) Conclusion: Fail to reject null. No significant difference.\n")
}

# (b) Create boxplots of the four strains for visual comparison
# (8 marks)
boxplot(survival ~ group, col = c("lightblue", "lightgreen", "lightpink", "orange"),
        main = "Survival Days by Strain", xlab = "Strain", ylab = "Survival Days")

# R-script name: Question-4-r.r  # Note: Modified from original "Question-4-r" for valid R filename

