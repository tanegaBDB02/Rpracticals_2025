# ===== Question 1 =====
# Script name: Question-1.r

# (a) Read "expression.txt" into data frame and print column names
# [2 marks]
data <- read.table("expression.txt", header = TRUE)
cat("Column Names:\n")
print(colnames(data))

# (b) For each gene:
#     1. Calculate mean of 8 controls ("control_mean")
#     2. Calculate mean of 6 treatments ("treatment_mean")
#     3. Compute RatioCol = control_mean/treatment_mean
#     4. Print first 10 elements of RatioCol
# [3 marks]
control_cols <- paste0("control", 1:8)
treatment_cols <- paste0("treatment", 1:6)

control_mean <- rowMeans(data[, control_cols])
treatment_mean <- rowMeans(data[, treatment_cols])
RatioCol <- control_mean / treatment_mean

cat("First 10 values of RatioCol:\n")
print(head(RatioCol, 10))


# (c) Create 2x2 plot layout with:
# (i) Histogram of control3
par(mfrow = c(2, 2))
hist(data$control3, main = "Histogram: control3", col = "lightblue", xlab = "Expression")

# (ii) Histogram of treatment3
hist(data$treatment3, main = "Histogram: treatment3", col = "lightgreen", xlab = "Expression")

# (iii) Scatter: control vs control3
plot(control_mean, data$control3, main = "control_mean vs control3", xlab = "control_mean", ylab = "control3", col = "blue")

# (iv) Scatter: control vs treatment3
plot(control_mean, data$treatment3, main = "control_mean vs treatment3", xlab = "control_mean", ylab = "treatment3", col = "red")
# [5 marks]


# ===== Question 2 =====
# Script name: Question-2.r

# (i) Create subset "sub1" where control1 > 50 AND control2 > 50
# [2 marks]
sub1 <- subset(data, control1 > 50 & control2 > 50)

# (ii) Create subset "subrow" containing rows for genes:
#      "gens6", "gens-20", "gens-37", "gens-100"
# [3 marks]
subrow <- data[rownames(data) %in% c("gens6", "gens-20", "gens-37", "gens-100"), ]

# (iii) Randomly sample 200 rows, save as "ssavn"
# [3 marks]
set.seed(123)  # for reproducibility
ssavn <- data[sample(nrow(data), 200), ]

# (iv) Boxplot comparing:
#      control4, control5, control6, treatments4, treatment5, treatments6
# [2 marks]
boxplot(data$control4, data$control5, data$control6, 
        data$treatment4, data$treatment5, data$treatment6,
        names = c("C4", "C5", "C6", "T4", "T5", "T6"),
        main = "Control vs Treatment Boxplots", col = rainbow(6))


# ===== Question 3 =====
# Script name: Question-3.r

# (i) Generate 10000 random deviates from N(μ=20, σ=4)
#     Plot histogram
# [3 marks]
set.seed(123)
rand_vals <- rnorm(10000, mean = 20, sd = 4)
hist(rand_vals, breaks = 50, col = "skyblue", main = "N(20, 4) Histogram")

# (ii) Compute and print Full Width at Half Maximum (FWHM)
# [3 marks]
fwhm <- 2.355 * 4  # For normal distribution: FWHM = 2.355 * sigma
cat("FWHM:", fwhm, "\n")

# (iii) For Gaussian N(μ=50, σ=6):
#       Compute area between x=50 and x=55 using unit normal
# [4 marks]
z1 <- (50 - 50) / 6
z2 <- (55 - 50) / 6

area <- pnorm(z2) - pnorm(z1)
cat("Area between 50 and 55 under N(50, 6):", area, "\n")


# ===== Question 4 =====
# Script name: Question-4.r

# (i) Plot two binomial distributions on same plot:
#     1. Binom(n=12, p=0.3) [blue]
#     2. Binom(n=13, p=0.7) [red]
# [5 marks]
x1 <- 0:12
x2 <- 0:13

binom1 <- dbinom(x1, size = 12, prob = 0.3)
binom2 <- dbinom(x2, size = 13, prob = 0.7)

plot(x1, binom1, type = "h", col = "blue", lwd = 2, ylim = c(0, max(binom1, binom2)),
     main = "Binomial Distributions", xlab = "x", ylab = "Probability")
lines(x2, binom2, type = "h", col = "red", lwd = 2)

legend("topright", legend = c("Binom(12, 0.3)", "Binom(13, 0.7)"),
       col = c("blue", "red"), lwd = 2)

# (ii) Generate 5000 random Poisson(μ=8) deviates
#      Plot frequency plot
#      Overlay Gaussian with same μ and σ
# [5 marks]
set.seed(123)
pois_data <- rpois(5000, lambda = 8)
hist(pois_data, breaks = 20, col = "lightgrey", freq = FALSE,
     main = "Poisson(8) with Normal Overlay", xlab = "x")

# Overlay Gaussian curve
x_vals <- seq(min(pois_data), max(pois_data), length.out = 100)
normal_curve <- dnorm(x_vals, mean = 8, sd = sqrt(8))
lines(x_vals, normal_curve, col = "blue", lwd = 2)