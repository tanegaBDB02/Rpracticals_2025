############################################################
# QUESTION 1
# The given text file called "expression.txt" contains expression data 
# of 10000 genes with 8 controls and 8 treatments from a study.
# You are asked to perform the following tasks:
############################################################

# (a) Read the data file "expression.txt" into a data-frame 
#     and print column names. [2 marks]

# Reading the file and printing the column names
expr_data <- read.csv("expression.txt", header = TRUE)
print(colnames(expr_data))

############################################################

# (b) For every gene, find the mean of all the 8 control values (call it "control_mean") 
#     and the mean of all the 8 treatment values (call it "treatment_mean") separately.
#     Then divide the control_mean by treatment_mean and name the resulting new column 
#     as "RatioCol". Print the first 10 elements of "RatioCol". [3 marks]

# Calculating average of 8 control columns (columns 2 to 9)
expr_data$control_mean <- rowMeans(expr_data[, 2:9])

# Calculating average of 8 treatment columns (columns 10 to 17)
expr_data$treatment_mean <- rowMeans(expr_data[, 10:17])

# Creating a new column RatioCol = control_mean / treatment_mean
expr_data$RatioCol <- expr_data$control_mean / expr_data$treatment_mean

# Printing the first 10 values of the RatioCol
print(head(expr_data$RatioCol, 10))

############################################################

# (c) Create the following four plots on the same page (canvas) 
#     by dividing the canvas into 2x2 [5 marks]
#     (i) Histogram of control3 column
#     (ii) Scatter plot between control3 and control4
#     (iii) Scatter plot between control3 and treatment6
#     (iv) Scatter plot between control2 and treatment5

# Splitting the plotting area into 2 rows and 2 columns
par(mfrow = c(2, 2))

# (i) Plotting histogram of control3
hist(expr_data$control3, 
     main = "Histogram of Control3", 
     xlab = "Control3", 
     col = "skyblue")

# (ii) Scatter plot: control3 vs control4
plot(expr_data$control3, expr_data$control4,
     main = "Control3 vs Control4", 
     xlab = "Control3", 
     ylab = "Control4", 
     col = "blue", 
     pch = 16)

# (iii) Scatter plot: control3 vs treatment6
plot(expr_data$control3, expr_data$treatment6,
     main = "Control3 vs Treatment6", 
     xlab = "Control3", 
     ylab = "Treatment6", 
     col = "green", 
     pch = 16)

# (iv) Scatter plot: control2 vs treatment5
plot(expr_data$control2, expr_data$treatment5,
     main = "Control2 vs Treatment5", 
     xlab = "Control2", 
     ylab = "Treatment5", 
     col = "purple", 
     pch = 16)


############################################################
# QUESTION 2
# For the same data file "expression.csv", perform the following
# operations in a script called "Question-2.r"
############################################################

# Reading the data first
expr_data <- read.csv("expression.csv", header = TRUE)

############################################################
# (a) Create a subset dataframe called "sub1" such that 
#     both the control4 and control2 values are greater than 50. [2 marks]

# Creating subset where control4 > 50 and control2 > 50
sub1 <- subset(expr_data, control4 > 50 & control2 > 50)

# Printing first few rows to check
print(head(sub1))

############################################################
# (b) Create another dataframe called "sub2" that contains rows 
#     of genes "gene9", "gene20", "gene37", and "gene100" [3 marks]

# Creating subset for specific gene names
sub2 <- subset(expr_data, gene %in% c("gene9", "gene20", "gene37", "gene100"))

# Printing sub2
print(sub2)

############################################################
# (c) Take the first 20 rows from the expression data and save 
#     them as a data frame called "samp" [3 marks]

# Selecting the first 20 rows
samp <- expr_data[1:20, ]

# Printing samp
print(samp)

############################################################
# (d) Create a vector called "cols" and assign these column names to it:
#     control1, control4, control5, control6, treatment4, treatment5 [2 marks]

# Creating vector of column names
cols <- c("control1", "control4", "control5", "control6", 
          "treatment4", "treatment5")

# Printing the vector
print(cols)


############################################################
# QUESTION 3
# Do the following in a script called "Question-3.r"
############################################################

# (a) Generate 500 random deviates from a Gaussian distribution 
#     of mean = 20, sd = 4 and plot the histogram [3 marks]

# Generating 500 random values from normal distribution (mean = 20, sd = 4)
set.seed(123)  # For reproducibility
normal_vals <- rnorm(500, mean = 20, sd = 4)

# Plotting histogram
hist(normal_vals, 
     main = "Histogram of N(20, 4)", 
     xlab = "Value", 
     col = "lightgreen")

############################################################
# (b) Compute the Full Width at Half Maximum for this data 
#     and print it [3 marks]

# Getting the density curve of the values
d <- density(normal_vals)

# Getting max height of the curve (peak)
peak <- max(d$y)
half_max <- peak / 2

# Finding where the curve crosses half max
left_idx <- which(d$y > half_max)[1]
right_idx <- tail(which(d$y > half_max), 1)

# Calculating FWHM
FWHM <- d$x[right_idx] - d$x[left_idx]
cat("Full Width at Half Maximum (FWHM) =", FWHM, "\n")

############################################################
# (c) For a Gaussian P(x, μ, σ) with mean = 30 and σ = 6, 
#     compute the area under the curve between x = 35 and x = 50 
#     using unit normal distribution. [4 marks]

# Standardizing values
z1 <- (35 - 30) / 6  # z for 35
z2 <- (50 - 30) / 6  # z for 50

# Calculating area under normal curve using pnorm
area <- pnorm(z2) - pnorm(z1)

# Printing the area
cat("Area under N(30,6) between x = 35 and x = 50 is", area, "\n")


############################################################
# QUESTION 4
# Consider the following survival data (time to event and censoring):
# time = 8, 6, 12, 20, 2, 15
# status = 1, 0, 1, 1, 0, 1
# (1 = event occurred, 0 = censored)
# Do the following using R:
############################################################

# (a) Create a data frame for the above values. [2 marks]

# Creating vectors
time <- c(8, 6, 12, 20, 2, 15)
status <- c(1, 0, 1, 1, 0, 1)

# Creating the data frame
surv_data <- data.frame(time = time, status = status)

# Printing the data
print(surv_data)

############################################################
# (b) Load the survival package and compute the Kaplan-Meier estimator. [3 marks]

# Loading the survival package
library(survival)

# Creating the survival object
surv_obj <- Surv(time = surv_data$time, event = surv_data$status)

# Fitting the Kaplan-Meier survival curve
km_fit <- survfit(surv_obj ~ 1)

# Printing the KM fit summary
summary(km_fit)

############################################################
# (c) Plot the Kaplan-Meier survival curve. [2 marks]

# Plotting the KM survival curve
plot(km_fit,
     main = "Kaplan-Meier Survival Curve",
     xlab = "Time",
     ylab = "Survival Probability",
     col = "blue",
     lwd = 2)

############################################################
# (d) Add confidence intervals to the above plot. [3 marks]

# Adding confidence intervals is enabled by default in plot
# But explicitly adding legend to clarify
legend("topright", 
       legend = c("Survival curve", "95% CI"), 
       col = c("blue", "black"), 
       lwd = c(2, 1), 
       lty = c(1, 2))
