
# (1) CLT case with sampling from non-normal distribution: Here we will demonstrate
# the central limit theorem by taking samples from a non-normal distribution such as the
# uniform distribution. Follow the steps below.
# (i) Generate a sample set of 5 random deviates from a uniform distribution in the
# interval [0,10]. Repeat this 10,000 times. We now have 10,000 samples of 5
# numbers each.
samples=replicate(10000,runif(5,min=0,max=10))
print(samples)

# (ii) Calculate the mean for each of the 10,000 samples and store it in a separate array.
# Plot this distribution of means as a histogram. Print the mean and standard
# deviation of the distribution of means.
sample_means=colMeans(samples)
print(sample_means)

hist(sample_means, breaks=30, col="rosybrown4", probability=TRUE,
     main="Uniform distribution",
     xlab="Sample Mean")

#print mean and standard deviation of sample means
mean_sample_means <- mean(sample_means)
sd_sample_means <- sd(sample_means)

print(paste("Mean of sample means:", mean_sample_means))
print(paste("Standard deviation of sample means:", sd_sample_means))

# (iii) Generate a sequence of numbers between 0 and 10 with 0.1 spacing. Obtain the
# normal probability densities using dnorm function with the calculated mean and
# standard deviation in the last question and store it as a separate vector.
x <- seq(0, 10, by = 0.1)
mean_value=mean_sample_means
std_value=sd_sample_means

normal_pdf_val=dnorm(x, mean=mean_value,sd=std_value)

plot(x, normal_pdf_val, type = "l", main = "Normal Density Curve", xlab = "x", ylab = "Density")

# (iv) Since we have 10,000 samples, we have to scale the normal probability density
# function to our particular case (since the area is 1.0 otherwise). The height and
# bin width of a histogram are related to each other – if we doubled the bin width,
# there would be roughly twice as many numbers in the bin and the bar would be
# twice as high on the y-axis. So to get the height of the bars on our frequency scale,
# we multiply the total frequency, i.e., 10,000 by the bin width 0.5 to get 5000. This
# is the scaling factor for the PDF. Perform this and save the scaled probabilities
# as a separate array.
scaling_factor=10000*0.5
scaled_pdf=normal_pdf_val*scaling_factor
print(scaled_pdf)

# (v) Plot the normal curve on top of the histogram to see the level of agreement
# between the normal behaviour of the sample means and the normal curve.
# Plot the histogram first with correct bin width and no default plot
hist(sample_means, breaks = 30, col = "lightblue",
     main = "Histogram of Sample Means with Normal Curve",
     xlab = "Sample Mean", ylab = "Frequency", freq = TRUE)

# Overlay the scaled normal curve
lines(x, scaled_pdf, col = "maroon", lwd = 2)

# Optional: Add a legend
legend("topright", legend = c("Histogram", "Scaled Normal Curve"),
       col = c("lightblue", "maroon"), lwd = c(10, 2), bty = "n")

# (2) CLT demo with sampling from non-normal, non-functional distribution: Here
# we demonstrate CLT by taking samples not from an intrinsic definition of the uniform
# distribution. Here we will create samples of dice throwing exercise.
# (i) Create 10,000 samples of single dice throw using the sample() function:
#   a<- sample(1:6,replace=T,10000)
# Make a plot of this distribution. You should see a uniform distribution.
a=sample(1:6,replace=T,10000)
barplot(table(a), col = "seagreen3", main = "One Die Throw (Uniform Distribution)",
        xlab = "Die Face", ylab = "Frequency")
  
# (ii) Throw two dice and add the scores together (this is the ancient game of craps).
# Generate a new object b similar to the above. Plot a histogram of a+b. You
# should see a triangular shape developing for the histogram.
# Throw another 10,000 dice and add to the previous
b <- sample(1:6, replace = TRUE, 10000)
sum_two_dice <- a + b

# Histogram for sum of two dice
hist(sum_two_dice, breaks = seq(1.5, 12.5, by = 1), col = "pink4",
     main = "Sum of Two Dice",
     xlab = "Sum", ylab = "Frequency")

# (iii) Repeat the exercise with three dice. The histogram should start showing the
# distinct bell shape.
# Add a third die
c <- sample(1:6, replace = TRUE, 10000)
sum_three_dice <- a + b + c

hist(sum_three_dice, breaks = seq(2.5, 18.5, by = 1), col = "plum",
     main = "Sum of Three Dice (Bell Shape Starts)",
     xlab = "Sum", ylab = "Frequency")

# (iv) Repeat this with five dice. The histogram is now very close to a normal curve.
# Use the mean and standard deviation of the 5 dice to generate a normal PDF. As
# in the last problem, one has to scale the PDF to match the height of the normal
# curve with the height of the histogram.
# Add fourth and fifth dice
d <- sample(1:6, replace = TRUE, 10000)
e <- sample(1:6, replace = TRUE, 10000)
sum_five_dice <- a + b + c + d + e

# Compute mean and sd of the 5-dice sum
mean_val <- mean(sum_five_dice)
sd_val <- sd(sum_five_dice)

# Plot histogram
hist_result <- hist(sum_five_dice, breaks = seq(4.5, 30.5, by = 1), col = "indianred",
                    main = "Sum of Five Dice (CLT: Approaching Normal)",
                    xlab = "Sum", ylab = "Frequency")

# Generate sequence and normal PDF
x_vals <- seq(5, 30, by = 0.1)
pdf_vals <- dnorm(x_vals, mean = mean_val, sd = sd_val)

# Scale PDF to match histogram
scaling_factor <- 10000 * 1  # bin width = 1, 10,000 samples
scaled_pdf <- pdf_vals * scaling_factor

# Overlay scaled normal curve
lines(x_vals, scaled_pdf, col = "navyblue", lwd = 2)

# Add legend
legend("topright", legend = c("Histogram", "Scaled Normal Curve"),
       col = c("indianred", "navyblue"), lwd = c(10, 2), bty = "n")


# IV. ROC curve
# Here we will learn to use the pROC library of R to perform ROC analysis. The function to
# be used from this library is plot.roc(). There are two datasets pertaining to red wine and
# white wine samples from the north of Portugal. The goal is to model wine quality based on
# physicochemical tests.
# There are 11 input variables (see column names) and the output variable is the quality score in
# scale of 0 to 10 from wine tasting. We want to test various thresholds of the quality score to
# achieve the best possible binary classifier of good and bad quality wine.
# Try the following:
#   
#(1) Read in the white wine data into a dataframe, and create additional columns that classifies 
#the data as good or bad wine based on threshold quality scores of 6, 7, 8, 9 and 10.

library(pROC)
# Read in the white wine dataset
wine_data <- read.csv("/home/ibab/R/winequality-white.csv", sep = ";")
colnames(wine_data)

#initialize an empty vector
good6 <- c()

for (i in 1:nrow(wine_data)) {
  quality_value <- wine_data$quality[i]

  if (quality_value >= 6) {
    good6[i] <- 1
  } else {
    good6[i] <- 0
  }
}
wine_data$good6 <- good6

good7 <- c()
for (i in 1:nrow(wine_data)) {
  if (wine_data$quality[i] >= 7) {
    good7[i] <- 1
  } else {
    good7[i] <- 0
  }
}
wine_data$good7 <- good7


good8 <- c()
for (i in 1:nrow(wine_data)) {
  if (wine_data$quality[i] >= 8) {
    good8[i] <- 1
  } else {
    good8[i] <- 0
  }
}
wine_data$good8 <- good8

good9 <- c()
for (i in 1:nrow(wine_data)) {
  if (wine_data$quality[i] >= 9) {
    good9[i] <- 1
  } else {
    good9[i] <- 0
  }
}
wine_data$good9 <- good9

good10 <- c()
for (i in 1:nrow(wine_data)) {
  if (wine_data$quality[i] == 10) {
    good10[i] <- 1
  } else {
    good10[i] <- 0
  }
}
wine_data$good10 <- good10
print (colnames(wine_data))
print((wine_data))

# (2) Use plot.roc() function as follows to plot the ROC curves for each threshold value.
# Which threshold value brings the ROC curve to the perfect classifier?
#   plot.roc(data$winequality,data$alcohol, #data vectors
#            main="title", #main plot title
#            legacy.axes=TRUE, # plots sensitivity against (1-specificity)
#            ci=TRUE, # plot confidence interval of AUC
#            print.auc=TRUE, # print values of AUC
#            identity.lwd=2, # linewidth of the 45 deg line (worst classifier)
#            print.thres=TRUE, # print best threshold on the graph


plot.roc(wine_data$good6, wine_data$alcohol,
         main="ROC Curve for Quality >= 6",
         legacy.axes=TRUE, ci=TRUE, print.auc=TRUE,
         identity.lwd=2, print.thres=TRUE)

plot.roc(wine_data$good7, wine_data$alcohol,
         main="ROC Curve for Quality >= 7",
         legacy.axes=TRUE, ci=TRUE, print.auc=TRUE,
         identity.lwd=2, print.thres=TRUE)

plot.roc(wine_data$good8, wine_data$alcohol,
         main="ROC Curve for Quality >= 8",
         legacy.axes=TRUE, ci=TRUE, print.auc=TRUE,
         identity.lwd=2, print.thres=TRUE)

plot.roc(wine_data$good9, wine_data$alcohol,
         main="ROC Curve for Quality >= 9",
         legacy.axes=TRUE, ci=TRUE, print.auc=TRUE,
         identity.lwd=2, print.thres=TRUE)

plot.roc(wine_data$good10, wine_data$alcohol,
         main="ROC Curve for Quality = 10",
         legacy.axes=TRUE, ci=TRUE, print.auc=TRUE,
         identity.lwd=2, print.thres=TRUE)


plot.roc(data$winequality,data$alcohol, #data vectors
         main="title", #main plot title
         legacy.axes=TRUE, # plots sensitivity against (1-specificity)
         ci=TRUE, # plot confidence interval of AUC
         print.auc=TRUE, # print values of AUC
         identity.lwd=2, # linewidth of the 45 deg line (worst classifier)
         print.thres=TRUE, # print best threshold on the graph
         
)









cat(readLines("/home/ibab/R/lab_10-11.R"), sep = "\n")


