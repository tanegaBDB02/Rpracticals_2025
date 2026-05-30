# 1.
# (a) Cardiac output (liters/minute) was measured by thermodilution in a random sample of 15 post cardiac surgical patients in the left lateral position. The results were as follows:
#   
#   4.91, 4.10, 6.74, 7.27, 7.42, 7.50, 6.56, 4.64, 5.98, 3.14, 3.23, 5.80, 6.17, 5.39, 5.77
# 
# We wish to know if we can conclude on the basis of this data that the population mean is different from 5.2. Write an R script to perform appropriate t-test for testing the hypothesis. Print the statistic value, p-value of the test and conclusions on the screen.
# [5 marks]

# Cardiac output data
cardiac_output <- c(4.91, 4.10, 6.74, 7.27, 7.42, 7.50, 6.56, 4.64, 5.98, 3.14, 3.23, 5.80, 6.17, 5.39, 5.77)

# Perform one-sample t-test
result <- t.test(cardiac_output, mu = 5.2)

# Print results
print(result)
cat("Conclusion: ", ifelse(result$p.value < 0.05, "Reject Null Hypothesis", "Fail to Reject Null Hypothesis"), "\n")

# (b) For the above data, write an R-script to compute a 90% Confidence Interval and print the results.
# [5 marks]
# 
# R-script name: Question-1.r
# 

# Compute 90% confidence interval
result_90 <- t.test(cardiac_output, mu = 5.2, conf.level = 0.90)

# Print the 90% confidence interval
print(result_90$conf.int)

# 
# 2.
# Drug A was prescribed for a random sample of 12 patients complaining of insomnia. An independent random sample of 16 patients with the same complaint received Drug B. The number of hours of sleep experienced during the second night after treatment began were as follows:
#   
#   Drug A:
#   3.5, 5.7, 3.4, 6.9, 17.8, 3.8, 3.0, 6.4, 6.8, 3.6, 6.9, 5.7
# 
# Drug B:
#   4.5, 11.7, 10.8, 4.5, 6.3, 3.8, 6.2, 6.6, 7.1, 6.4, 4.5, 5.1, 3.2, 4.7, 4.5, 3.0
# 
# (a) Assuming both the populations follow Gaussian distributions with unknown and equal means, write an R script to perform a two-sample Student’s t-test to check for the equality of the populations means. Take α = 0.05 for accepting or rejecting the null hypothesis. Print the value of the t-statistic and the p-value of the test. Also, print the conclusion of the test in a line.
# [5 marks]

# Data for Drug A and Drug B
drugA <- c(3.5, 5.7, 3.4, 6.9, 17.8, 3.8, 3.0, 6.4, 6.8, 3.6, 6.9, 5.7)
drugB <- c(4.5, 11.7, 10.8, 4.5, 6.3, 3.8, 6.2, 6.6, 7.1, 6.4, 4.5, 5.1, 3.2, 4.7, 4.5, 3.0)

# Perform two-sample Student's t-test (equal variances)
result_ttest <- t.test(drugA, drugB, var.equal = TRUE)

# Print results
print(result_ttest)
cat("Conclusion: ", ifelse(result_ttest$p.value < 0.05, "Reject Null Hypothesis", "Fail to Reject Null Hypothesis"), "\n")


# 
# (ii) For the same data above, perform a two-sample Welch’s t-test to check for the equality of population means, and print the results. Comment on whether both the test yields similar results.
# [5 marks]

# Perform two-sample Welch's t-test (unequal variances)
result_welch <- t.test(drugA, drugB)

# Print results
print(result_welch)
cat("Conclusion: ", ifelse(result_welch$p.value < 0.05, "Reject Null Hypothesis", "Fail to Reject Null Hypothesis"), "\n")


# 
# 3.
# The following data shows the relationship between chirps per second of a striped ground cricket and the corresponding ground temperature measured in a forest.
# 
# Chirps/second	14.7	19.8	18.4	17.1	15.5	19.5
# Temperature(°F)	69.7	93.3	84.3	80.6	75.2	90.0
# Fit a least square regression line to this data by treating ground temperature as the independent variable.
# 
# Print the values of slope and intercepts as legends on the plot.
# [10 marks]

# Data
chirps <- c(14.7, 19.8, 18.4, 17.1, 15.5, 19.5)
temperature <- c(69.7, 93.3, 84.3, 80.6, 75.2, 90.0)

# Linear regression model
model <- lm(temperature ~ chirps)

# Print slope and intercept
coefficients <- coef(model)
cat("Intercept:", coefficients[1], "Slope:", coefficients[2], "\n")

# Plot with regression line
plot(chirps, temperature, main="Chirps vs Temperature", xlab="Chirps per Second", ylab="Temperature (°F)")
abline(model, col="blue")
legend("topleft", legend=paste("y =", round(coefficients[2],2), "* x +", round(coefficients[1],2)), col="blue", lwd=2)

# 
# 4.
# (a) A group of mice was injected with 4 strains of some organism to observe the number of days each mouse survived. The results are presented here:
#   
#   Strain 1: 11, 10, 9, 12, 13, 11
# 
# Strain 2: 7, 8, 9, 7, 6, 6
# 
# Strain 3: 9, 8, 10, 11, 10, 10
# 
# Strain 4: 12, 11, 13, 14, 12, 13
# 
# Write an R script to perform a one-way ANOVA to find whether the mean effect of the 4 different strains is the same. Let α = 0.04 for the test.
# 
# Print the following information on the screen:
#   (i) Statement of null hypothesis
# (ii) Value of F-statistics
# (iii) p-value of the test
# (iv) Conclusion of the test in a line
# [7 marks]

# Data
strain1 <- c(11, 10, 9, 12, 13, 11)
strain2 <- c(7, 8, 9, 7, 6, 6)
strain3 <- c(9, 8, 10, 11, 10, 10)
strain4 <- c(12, 11, 13, 14, 12, 13)

# Combine into data frame
days <- c(strain1, strain2, strain3, strain4)
strain <- factor(rep(c("Strain1", "Strain2", "Strain3", "Strain4"), each=6))

# Perform ANOVA
anova_result <- aov(days ~ strain)

# Print results
summary_result <- summary(anova_result)
print(summary_result)

# Extract p-value
p_value <- summary_result[[1]][["Pr(>F)"]][1]

# Output conclusion
cat("Null Hypothesis: The mean survival days are the same across strains.\n")
cat("F-Statistic:", summary_result[[1]][["F value"]][1], "\n")
cat("p-value:", p_value, "\n")
cat("Conclusion: ", ifelse(p_value < 0.04, "Reject Null Hypothesis", "Fail to Reject Null Hypothesis"), "\n")



# Boxplot
boxplot(strain1, strain2, strain3, strain4,
        names=c("Strain1", "Strain2", "Strain3", "Strain4"),
        main="Survival Days of Mice by Strain",
        ylab="Number of Days Survived",
        col=c("skyblue","lightgreen","orange","pink"))












