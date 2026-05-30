#q1 (a) For a = 22.5, b = 51.6 and c = 11.4, compute Q according to the following relation and print it:
# Given values
a <- 22.5
b <- 51.6
c <- 11.4

# Calculate Q using the provided formula
Q <- (sqrt(a^2 + b^2 + c^2) / (a + b + c)) * exp(b / (a + c))

# Print the result
print(Q)

#(b)(b) Create two vectors X and Y with the elements listed here:
#X = {12, 34, 9, 32, 24, 43, 52, 16, 2, 17, 39, 57, 72, 82, 92, 4, 59, 79, 99}
#Y = {33, 24, 15, 49, 22, 48, 66, 10, 4, 24, 42, 50, 66, 75, 99, 9, 61, 72, 90}
#Create a vector called "Xsmall" by filtering numbers higher than 50 from X. Print the elements of "Xsmall".
#Similarly, create a vector called "Ywindow" by selecting elements more than 20 and less than 70 from Y. Print the elements of "Ywindow". Filter elements from vectors X and Y:
# Given vectors
X <- c(12, 34, 9, 32, 24, 43, 52, 16, 2, 17, 39, 57, 72, 82, 92, 4, 59, 79, 99)
Y <- c(33, 24, 15, 49, 22, 48, 66, 10, 4, 24, 42, 50, 66, 75, 99, 9, 61, 72, 90)

# Create Xsmall - filter X for elements > 50
Xsmall <- X[X > 50]
print("Elements of Xsmall:")
print(Xsmall)

# Create Ywindow - filter Y for elements > 20 and < 70
Ywindow <- Y[Y > 20 & Y < 70]
print("Elements of Ywindow:")
print(Ywindow)
############
#Question 2:
#(a) Read the given data file “sample data.csv” into a data frame called “dat” and print the frame.
# Read the CSV file
dat <- read.csv("sample_data.csv")

# Print the data frame
print("Data frame 'dat':")
print(dat)

#(b) Create a subset of “dat” by removing the value above 800 from “standard sample” column. Called this subset frame as “subdat” and print it.
# Create a subset with values <= 800 in 'standard sample' column
subdat <- subset(dat, dat$`standard sample` <= 800)
print("Subset 'subdat':")
print(subdat)

#(c) Divide the canvas into 2 portions and plot the histograms of standard sample and new sample columns separately with blue and red colors respectively.
# Split canvas into 2 parts (1 row, 2 columns)
par(mfrow = c(1, 2))

# Plot histogram for 'standard sample' in blue
hist(dat$`standard sample`, col = "blue", main = "Standard Sample", xlab = "Value")

# Plot histogram for 'new sample' in red
hist(dat$`new sample`, col = "red", main = "New Sample", xlab = "Value")

############
#Question 3:
#(a) Generate 9000 random deviates from a Gaussian distribution with mean 16 and standard deviation 3.
# Generate 9000 values from a normal distribution
data <- rnorm(9000, mean = 16, sd = 3)

#(b) Using R library functions, compute the minimum, median, mean, variance and maximum of this data set and print them.
# Use R functions to compute statistics
cat("Minimum:", min(data), "\n")
cat("Median:", median(data), "\n")
cat("Mean:", mean(data), "\n")
cat("Variance:", var(data), "\n")
cat("Maximum:", max(data), "\n")

#(c) Divide this data into two equal subsets “D1” and “D2”. Plot the scatter plot between these two vectors.
# Divide into two equal subsets
D1 <- data[1:4500]
D2 <- data[4501:9000]

# Scatter plot
plot(D1, D2, main = "Scatter plot of D1 vs D2", xlab = "D1", ylab = "D2", col = "purple", pch = 16)


#############
# QUESTION 4
# Drug A was prescribed to 12 patients. Drug B to 16 patients.
# The number of hours of sleep experienced on the second night is recorded.

# Drug A: 3.5, 5.7, 3.4, 6.9, 17.8, 3.8, 3.0, 6.4, 6.8, 3.6, 6.9, 5.7
# Drug B: 4.5, 11.7, 10.8, 4.5, 6.3, 3.8, 6.2, 6.6, 7.1, 6.4, 4.5, 5.7, 3.2, 4.7, 4.5, 3.0

# (a) Perform two-sample t-test to compare the means (α = 0.05)
#     Print the t-value, p-value, and conclusion. [4 marks]
# (b) Compute 95% confidence interval for difference in means. [3 marks]


# Creating the vectors
drugA <- c(3.5, 5.7, 3.4, 6.9, 17.8, 3.8, 3.0, 6.4, 6.8, 3.6, 6.9, 5.7)
drugB <- c(4.5, 11.7, 10.8, 4.5, 6.3, 3.8, 6.2, 6.6, 7.1, 6.4, 4.5, 5.7, 3.2, 4.7, 4.5, 3.0)

# Performing two-sample t-test (assuming equal variances)
t_result <- t.test(drugA, drugB, var.equal = TRUE)

# Printing t-statistic and p-value
cat("t-value:", t_result$statistic, "\n")
cat("p-value:", t_result$p.value, "\n")

# Printing conclusion based on alpha = 0.05
if (t_result$p.value < 0.05) {
  cat("Conclusion: Reject null hypothesis. Means are significantly different.\n")
} else {
  cat("Conclusion: Fail to reject null. No significant difference.\n")
}

# (b) Printing 95% confidence interval
cat("95% Confidence Interval for mean difference:\n")
print(t_result$conf.int)


#################
## QUESTION 5
# A group of mice was injected with 4 strains of an organism.
# The number of days each mouse survived was recorded.

# Data:
# Strain 1 : 11, 10, 9, 12, 13, 11
# Strain 2 : 7, 8, 9, 7, 6, 6
# Strain 3 : 9, 8, 10, 11, 10, 10
# Strain 4 : 12, 11, 13, 14, 12, 13

# Task: Perform a one-way ANOVA to check if the mean number of days is the same.
# Use α = 0.04.
# Print:
# - Statement of null hypothesis
# - F-statistic
# - p-value
# - Conclusion in one line


# Creating the data
strain <- c(rep("Strain1", 6), rep("Strain2", 6), rep("Strain3", 6), rep("Strain4", 6))
days <- c(11, 10, 9, 12, 13, 11,
          7, 8, 9, 7, 6, 6,
          9, 8, 10, 11, 10, 10,
          12, 11, 13, 14, 12, 13)

# Making it a dataframe
data <- data.frame(strain = as.factor(strain), days = days)

# Performing one-way ANOVA
anova_result <- aov(days ~ strain, data = data)
summary_result <- summary(anova_result)

# Printing hypothesis
cat("Null Hypothesis: Mean survival days are equal for all 4 strains.\n")

# Printing F-statistic and p-value
f_value <- summary_result[[1]][["F value"]][1]
p_value <- summary_result[[1]][["Pr(>F)"]][1]
cat("F-statistic:", f_value, "\n")
cat("p-value:", p_value, "\n")

# Conclusion based on alpha = 0.04
if (p_value < 0.04) {
  cat("Conclusion: Reject null hypothesis. At least one strain has a different mean.\n")
} else {
  cat("Conclusion: Fail to reject null. All strains may have similar means.\n")
}

#Question 6. 
#Chemists use Ion Sensitive Electrodes to measure ionic concentrations of aqueous solutions.
#In order to calibrate this equipment, the output signal in millivolt was measured for known ion concentrations in units of ppm. The data is reproduced here:
  
#concentration (in ppm): 0.0 50.0 75.0 100.0 150.0 200.0
#signal (in mV): 1.72 2.11 2.36 2.56 3.05 3.42

#Calculate a least square regression line between concentration and signal data.
#Print the slope and intercept of the regression line.
#Plot the original data points and the regression line on the same plot.
# Define the data
concentration <- c(0.0, 50.0, 75.0, 100.0, 150.0, 200.0)
signal <- c(1.72, 2.11, 2.36, 2.56, 3.05, 3.42)

# Fit a linear regression model: signal ~ concentration
model <- lm(signal ~ concentration)

# Print slope and intercept
cat("Intercept:", coef(model)[1], "\n")    # Intercept (beta_0)
cat("Slope:", coef(model)[2], "\n")        # Slope (beta_1)

# Plot the original data points
plot(concentration, signal, 
     main = "Signal vs Concentration with Regression Line",
     xlab = "Concentration (ppm)", 
     ylab = "Signal (mV)",
     pch = 19, col = "blue")

# Add the regression line
abline(model, col = "red", lwd = 2)

# Optional: Add grid
grid()

######
#In order to test the effectiveness of a training program, employees of a tech company were made to take a technical test before and after the training program. Their test scores in some scale are listed below:
#Subject: 1 2 3 4 5 6 7 8 9 10 11
#Score before training: 43 36 46 35 41 37 39 40 45 32 37
#Score after training: 44 38 41 39 34 41 39 34 47 30 39

#Perform Wilcoxon signed rank test on this data to determine whether training program increases the test score to a significance level of 0.05.
#Print the null and alternate hypothesis clearly along with the conclusion of the test.
# Question-7.r

# Scores before and after training
before <- c(43, 36, 46, 35, 41, 37, 39, 40, 45, 32, 37)
after <- c(44, 38, 41, 39, 34, 41, 39, 34, 47, 30, 39)

# Perform Wilcoxon signed-rank test
test_result <- wilcox.test(after, before, 
                           paired = TRUE, 
                           alternative = "greater")

# Hypotheses
cat("Null Hypothesis (H0): The median difference is less than or equal to 0.\n")
cat("Alternate Hypothesis (H1): The training program increases the test score (median difference > 0).\n")

# Print test statistics
cat("Wilcoxon Test Statistic (V):", test_result$statistic, "\n")
cat("p-value:", test_result$p.value, "\n")

# Conclusion
if (test_result$p.value < 0.05) {
  cat("Conclusion: Reject H0. The training program significantly increases test scores.\n")
} else {
  cat("Conclusion: Fail to reject H0. No significant increase in test scores.\n")
}

