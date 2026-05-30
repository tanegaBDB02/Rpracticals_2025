#PART B - B1

#i)
drugA <- c(3.5,5.7,3.4,6.9,17.8,3.8,3.0,6.4,6.8,3.6,6.9,5.7)
drugB <- c(4.5,11.7,10.8,4.5,6.3,3.8,6.2,6.6,7.1,6.4,4.5,5.1,3.2,4.7,4.5,3.0)

#performing two-sample t-test that means equal variances
result_ttest <- t.test(drugA, drugB, var.equal=TRUE)

print(result_ttest)
cat("Conclusion: ",
    ifelse(result_ttest$p.value < 0.05, "reject null hypothesis",
           "fail to reject null hypothesis"), "\n")


#ii)
#performing two-sample Welch's t-test that means unequal variances
result_welch <- t.test(drugA,drugB)

print(result_welch)
cat("Conclusion: ",
    ifelse(result_welch$p.value < 0.05,
                           "reject null hypothesis",
                           "fail to reject null hypothesis"), "\n")

cat("\nComparison:\n")
cat("The p values differ slightly due to the different assumptions about variance.\n")

