# I. Error bars, covariance and correlation


# An error is a small line segment around each point (X,Y) representing the uncertainity in the
# measured value Y. The bar might be any one of the following: (a) standard deviation sy, (b)
# standard error of the mean σy/n orsy/ √n and (c) confidence interval around the (unknown)
# population mean given by Z1−α/2σy/ √n or if the population variance is not known, T1−α/2sy/√n.
# 
# For this we will use the arrows() function:
#   arrows(x0, y0, x1, y1, length, angle, code = 2, col, lty, lwd ) where the arrow is
# drawn from (x0,y0) to (x1,y1), with given length, angle between the arrow head and arrow
# head, and code to specify whether arrow heads at one end or both.


# (1)

#Error bars on bar plots: Enter the following data into R:
#   means = c(20.34,19.49,25.68)
# stderr = c(0.83,1.51,1.39)
# Make a barplot with the following features: the three means should be labeled as ‘A’,
# ‘B’ and ‘C’, grey filled bars, plot title as ’Errors on bar plot’. Use the arrows() function
# to plot the error bars as follows:
#   arrows(<barplotobject>, means+stderr, <barplotobject>,means-stderr,
#          angle=90,code=3,length=0.06,col=’red’)

means=c(20.34,19.49,25.68)
stderr=c(0.83,1.51,1.39)

labels<-c("A","B","C")

bp<-barplot(means,
            names.arg=labels,col="gray",
            ylim=c(0,max(means+stderr)+2),
            main="Errors on bar plot")

arrows(bp,means+stderr,bp,means-stderr,
       angle=90,code=3,length=0.06,col='red')

# (2)

#Error bars on (x,y) plots: Enter the following data into R. The errors provided are
# standard errors on the mean
# x = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
# y = c(5, 9, 13, 17, 20, 24, 26, 27, 28, 27)
# errors = c(0.5, 0.9, 1.4, 1.5, 2.0, 2.2, 2.3, 2.5, 2.9, 3.0)
# Plot (x,y) with points and xlabel ‘concentration’ and ylabel ‘optical activity’ and plot title
# as ‘Error bars on data points”. Again we can use the arrows() function to plot the error
# bars, only difference is in the first 4 arguments: arrows(x,y+errors,x,y-errors,.....)

x=c(5,10,15,20,25,30,35,40,45,50)
y=c(5,9,13,17,20,24,26,27,28,27)
errors=c(0.5,0.9,1.4,1.5,2.0,2.2,2.3,2.5,2.9,3.0)

plot(x,y,col="lightpink3",lwd=1.5,pch=19,
     main="Error bars on data points",
     xlab="Concentration",ylab="Optical Activity",ylim=c(min(y-errors),max(y+errors)))

arrows(x,y+errors,x,y-errors,angle=90,code=3,length=0.06,col='red')


#(3) 

# Covariance and Pearson’s correlation coefficient: For a univariate sample, the functions
# cov() and cor() return a number, for multivariate samples, these functions returns a
# matrix. Try the following:

x =c(10,20,30,40,50,60,70,80,90,100)
y=c(5, 220, 279, 424, 499, 540, 720, 880, 950, 1200)
cov(x,y)
cor(x,y)
cor(longley) #multivariate data

#II. One sample tests

#(1) One sample Z test:

# (a) Write a function to perform one sample Z test, where given a data set x that is
# randomly drawn from a Gaussian distribution of population mean μ and standard
# deviation σ the function returns the conclusions of the test along with the computed
# statistic values. The function should be defined as
# one_sample_Ztest(x,sigma,muzero, alpha,null) where x is the data vector,
# sigma is the population standard deviation, muzero is the population mean for
# comparison, alpha is the significance level and null is a string indicating type of
#null hypothesis. The possible values of null should be equal, less_than_or_equal
# or more_than_or_equal. The function should return a vector with two numbers
# and the conclusion: p-value and the Z-value and the statistical conclusion.

# (b) Use the data set to test the null hypothesis that μ = μ0:
#   x = c(141.5, 152.3, 121.2, 123.0, 151.6, 124.8, 138.9,
#         137.4, 145.6, 135.6, 135.4, 121.5)
# and μ0 of 124.6 and σ = 14.5 with 0.05 significance level.

one_sample_Ztest=function(x,sigma,muzero,alpha,null){
  xbar=mean(x)
  n=length(x)
  z=(xbar-muzero)/(sigma/sqrt(n))
  alpha=alpha/2
  
  if(null=="equal"){
    p<-2*(1-pnorm(abs(z))) #two-tailed test
  }else if(null=="less_than_or_equal"){
    p<-1-pnorm(z)
  }else if(null=="more_than_or_equal"){
    p<-pnorm(z)
  }else{
    stop("Invalid 'null' argument. Choose from 'equal', 'less_than_or_equal', 'more_than_or_equal'.")
  }
  
  conclusion<-ifelse(p<alpha,"Reject null hypothesis","Fail to reject null hypothesis")
  return(list("Z-value"=z,"p-value"=p,"Conclusion"=conclusion))
}

#sample data
x<-c(141.5,152.3,121.2,123.0,151.6,124.8,138.9,
     137.4,145.6,135.6,135.4,121.5)
muzero=124.6
sigma=14.5
alpha=0.05
null="equal"

result<-one_sample_Ztest(x,sigma,muzero,alpha,null)
print(result)
#since it is two-tailed test, the alpha value should be 0.05/2=0.025 and accordingly the p value
#will also be divided by 2

#(2) One sample t- test:

# (a) Write a function to perform a one sample t-test given a data set x that is randomly
# drawn from a Gaussian distribution of population mean μ and standard deviation
# σ. The function should return the conclusions of the test along with the statistic
# values. Function should be defined as
# one_sample_t_test(x,muzero,alpha,null) where the arguments have the same
# meaning as above.

# (b) Use the data set below to test the null hypothesis that μ = μ0, where μ0 = 100 for
# 0.05 significance level.
# x = c(96.0, 104.0, 99.1, 97.6, 99.4, 92.8, 105.6, 97.2,
#       96.8, 92.1, 100.6, 101.5, 100.7, 97.3, 99.6, 105.9)


one_sample_t_test=function(x,muzero,alpha,null){
  xbar=mean(x)
  n=length(x)
  s=sd(x)
  
  #test statistic
  t_stat=(xbar-muzero)/(s/sqrt(n))
  df=n-1 #degrees of freedom
  
  #calculation of p value
  if(null=="equal"){
    p<-2*(1-pt(abs(t_stat),df))
  }else if(null=="less_than_or_equal"){
    p<-1-pt(t_stat,df)
  }else if(null=="more_than_or_equal"){
    p<-pt(t_stat,df)
  }else{
    stop("Invalid 'null' argument. Use 'equal', 'less_than_or_equal', or 'more_than_or_equal'.")
  }
  
  conclusion<-ifelse(p<alpha,"Reject null hypothesis","Fail to reject null hypothesis")
  return(list(T_value=t_stat,p_value=p,Conclusion=conclusion))
}

x=c(96.0,104.0,99.1,97.6,99.4,92.8,105.6,97.2,
    96.8,92.1,100.6,101.5,100.7,97.3,99.6,105.9)
μ0=100
alpha=0.05
null="equal"

result=one_sample_t_test(x,muzero,alpha,null)
print(result)

# (3) One sample proportion test: In R, the functions binom.test() and prop.test() per-
# forms the one sample proportion test. The former computes the exact binomial probabil-
# ity, and is used when the sample sizes are small. The latter uses a normal approximation
# to the binomial distribution and can be used when n > 30. The functions are
# binom.test(x,n,p,alternative) prop.test(x,n,p,alternative,correct)
# where x is the number of successes, n is the total number of trials, p proportion to test
# against (i.e., hypothesized value), alternative is the string value indicating type of null
# hypothesis as “two-sided”, “less”, “greater”, and correct is a logical variable indicating
# whether a correction should be applied for small sample sizes (the default is TRUE).
# Print the results of both the tests with x = 710, n = 2600, p = 0.25 and alternative=greater.

binom.test(x=710,n=2600,p=0.25,alternative="greater")
prop.test(x=710,n=2600,p=0.25,alternative="greater")


#(4) One sample variance test:

# (a) Write a function with the following structure to output the statistical conclusion
# and properties (p-value and σ2 value):
# one_sample_variance_test(x,test_sigma,alpha), where the function should
# compute the χ2,test statistic and get the appropriate limits by using the qchisq()function 
# and deciding the conclusion.

# (b) Perform the above test for the data set given below for hypothesized σ = 29 and for
# 0.05 significance level. The points are from a normal distribution with mean=140
# and standard deviation of 20.
# x = c(142.8, 135.0, 157.5, 148.4, 135.9, 153.4, 149.0, 130.2,
#       156.0, 189.7, 151.6, 156.5, 123.8, 152.9, 118.4, 145.8)


one_sample_variance_test<-function(x,test_sigma,alpha){
  n<-length(x)
  sample_var<-var(x)
  
  #calculating the chi-squared test statistic
  test_stat<-(n-1)*sample_var/(test_sigma^2)
  
  #getting the chi-squared critical values for a two-tailed test
  lower_crit<-qchisq(alpha/2,df=n-1)
  upper_crit<-qchisq(1-alpha/2,df=n-1)
  
  #computing the two-tailed p-value
  p_value<-2*min(pchisq(test_stat,df=n-1),1-pchisq(test_stat,df=n-1))
  
  #deciding the conclusion based on the test statistic and critical values
  conclusion<-if(test_stat<lower_crit|test_stat>upper_crit){
    paste("rejecting H0: variance is significantly different from",test_sigma^2)
  }else{
    paste("not rejecting H0: variance is not significantly different from",test_sigma^2)
  }
  
  return(list(
    sample_variance=sample_var,
    test_statistic=test_stat,
    p_value=p_value,
    lower_critical_value=lower_crit,
    upper_critical_value=upper_crit,
    conclusion=conclusion
  ))
}

#printing the result
x<-c(142.8,135.0,157.5,148.4,135.9,153.4,149.0,130.2,156.0,189.7,151.6,156.5,123.8,152.9,118.4,145.8)
result<-one_sample_variance_test(x,test_sigma=29,alpha=0.05)
print(result)


# (5) One sample Wilcoxon signed rank test: In R, the function wilcox.test() carries out
# one- and two-sample non-parametric tests. The arguments are
# wilcox.test(x,y=NULL,alternative,mu=0,paired=FALSE,
#             exact=NULL,correct=TRUE,conf.int=FALSE,conf.level=0.95)
# where for two samples, y will not be NULL. Perform this test for the data set with μ = 160
# and confidence level of 95% and H0 as μ >= μ0.
# x = c(176.9, 158.3, 152.1, 158.8, 172.4, 169.8, 159.7, 162.7,
#       156.6, 174.5, 184.4, 165.2, 147.8, 177.8, 160.1, 161.5)


wilcoxon_signed_rank_test<-function(x,mu,conf_level){
  result<-wilcox.test(x,y=NULL,mu=mu,alternative="less",paired=FALSE,
                      exact=NULL,correct=TRUE,conf.int=FALSE,conf.level=0.95)
  return(result)
}

x<-c(176.9,158.3,152.1,158.8,172.4,169.8,159.7,162.7,156.6,174.5,184.4,165.2,147.8,177.8,160.1,161.5)
result<-wilcoxon_signed_rank_test(x,mu=160,conf_level=0.95)
print(result)


#III. Two sample tests

#(1) Two sample Z test:
  
# (a) Write a function to perform a two sample Z test given data sets x1, x2, their respec-
# tive standard deviations, siginficance level and null hypothesis. As above, the func-
#   tion should return the conclusions of the test along with the statistic values. The
# function should be defined as two_sample_Z_test(x1,x2,sigma_x1,sigma_x2,alpha,null)
# (b) Use the data given in two-sample.dat for this problem with σx1 = 24.6 and
# σx2 = 27.8 with α = 0.05 to test the null hypothesis that the μ1 ≥ μ2.


two_sample_Z_test<-function(x1,x2,sigma_x1,sigma_x2,alpha,null){
  n1<-length(x1)
  n2<-length(x2)
  mean_x1<-mean(x1)
  mean_x2<-mean(x2)

  z_score<-(mean_x1-mean_x2)/sqrt((sigma_x1^2/n1)+(sigma_x2^2/n2))
  #determining critical value based on null hypothesis
  if(null=="greater"){
    p_value<-pnorm(z_score,lower.tail=FALSE)
    critical_value<-qnorm(1-alpha)
    alternative<-"μ1>μ2"
  }else if(null=="less"){
    p_value<-pnorm(z_score)
    critical_value<-qnorm(alpha)
    alternative<-"μ1<μ2"
  }else{
    p_value<-2*pnorm(-abs(z_score))
    critical_value<-qnorm(c(alpha/2,1-alpha/2))
    alternative<-"μ1≠μ2"
  }

  if(grepl("greater",null)){
    decision<-ifelse(z_score>critical_value,"Reject","Fail to reject")
  }else if(grepl("less",null)){
    decision<-ifelse(z_score<critical_value,"Reject","Fail to reject")
  }else{
    decision<-ifelse(abs(z_score)>abs(critical_value[2]),"Reject","Fail to reject")
  }

  return(list(
    z_statistic=z_score,
    p_value=p_value,
    critical_value=critical_value,
    decision=decision,
    alternative=alternative,
    conclusion=paste(decision,"null hypothesis that",null)
  ))
}

data<-read.csv("/home/ibab/R/two-sample.csv",header=TRUE)
x1<-data$sample1
x2<-data$sample2

result<-two_sample_Z_test(
  x1=x1,
  x2=x2,
  sigma_x1=24.6,
  sigma_x2=27.8,
  alpha=0.05,
  null="greater"
)

cat("Two-Sample Z-Test Results:\n")
cat("Z-statistic:",result$z_statistic,"\n")
cat("p-value:",result$p_value,"\n")
cat("Critical value:",result$critical_value,"\n")
cat("Decision:",result$decision,"H0\n")
cat("Conclusion:",result$conclusion,"\n")

#(2) Two sample t-test: In R, both one-sample and two-sample t-tests can be performed with
# the library function t.test() with the following arguments:
#   t.test(x,y,alternative,mu,paired,var.equal,conf.level) where x,y are the data
# vectors, alternative is a character string specifying the alternate hypothesis of 3 possible values:
# two.sided, less and greater. One can also input a vector with all three,
# in which case all the 3 hypotheses are tested, the default value is two.sided. For one-
# sample, mu is the hypothesized mean of the population, and for two samples, it is the
# hypothesized difference of the means. A conf.level=0.95 sets the confidence level at 95%.

# (a)Welsch’s test: use the data sets to carry out the t-test for equality of means as H0.
# Print the results summary.
# Xvar=c(4.95,5.37,4.70,4.96,4.72,5.17,5.28,5.12,5.26,5.48)
# Yvar=c(4.65,4.86,4.57,4.56,4.96,4.63,5.04,4.92,5.37,4.58,4.26,4.40)


Xvar <- c(4.95, 5.37, 4.70, 4.96, 4.72, 5.17, 5.28, 5.12, 5.26, 5.48)
Yvar <- c(4.65, 4.86, 4.57, 4.56, 4.96, 4.63, 5.04, 4.92, 5.37, 4.58, 4.26, 4.40)

t_test_result <- t.test(Xvar, Yvar)
print(t_test_result)

# (b) Dependent variables: Use the data sets below to carry out a paired t-test for a
# significance level of 0.05. In this case, do we need to input μ?
# data_before = c(95,106,79,71,90,79,71,77,103,103,92,63,82,76)
# data_after = c(97,116,82,81,82,86,107,86,94,91,85,98,91,87)

data_before <- c(95, 106, 79, 71, 90, 79, 71, 77, 103, 103, 92, 63, 82, 76)
data_after <- c(97, 116, 82, 81, 82, 86, 107, 86, 94, 91, 85, 98, 91, 87)

paired_test_result <- t.test(data_before, data_after, paired = TRUE)
print(paired_test_result)


# (3) Two-sample proportion test: In R, the function prop.test() can perform proportion
# tests for one, two or more proportions. Here we will also learn the Fisher’s test applicable
# for small samples. The input is changed as follows:
# prop.test(x,n,p=NULL,alternative="two.sided",correct=TRUE)
# fisher.test(x,alternative="two.sided",conf.int=TRUE,conf.level=0.95)
# where x is a 2x2 matrix containing the values of the contingency table under different categories 
# for the Fisher’s test and a data vector of counts of successes for the prop.test(),
# n is a data vector containing number of trials in which x successes were observed, p is a
# vector of probabilites of successes. The alternative can be two.sided, less or more.

# (a) Perform a prop.test() test for the following problem with H0 that the proportions
# are equal. In a health survey, 520 out of 600 men and 550 out of 600 women
# questioned said they use antibiotics whenever fever continues for more than 2 days.
# We want to test whether there is a significant difference in the fraction of men and
# women who start taking antibotics after 2 days of fever.

successes <- c(520,550)#number of successes (people who take antibiotics after 2 days)
totals <- c(600,600)#total number of people surveyed in each group
prop_test_result <- prop.test(successes,totals)
print(prop_test_result)


# (b) Perform a Fisher’s exact test for the following data to find whether the patients from
# higher income group indulge in tobacco abuse in a significantly different proportion
# than the patients from the lower income group.
# 
#             Higher-income Lower-income
# Tobacco abuse 11            17
# No abuse      42            39

data_matrix=matrix(c(11,17,42,39),nrow=2,byrow=TRUE)

rownames(data_matrix)=c("Tobacco_abuse","No_abuse")
colnames(data_matrix)=c("Higher_income","Lower_income")

fisher_result=fisher.test(data_matrix)
print(fisher_result)


# (4) Two-sample variance test: In R, we will use the F-distribution functions (qf()) to carry
# out the two-sample variance test.
# 
# (a) Write a function of the form two_sample_variance_test(x,y,alpha) that out-
#   puts the statistical conclusion along with the statistical values of F and p-values.


two_sample_variance_test <- function(x,y,alpha=0.05){
  var_x<-var(x)
  var_y<-var(y)
  
  df_x<-length(x)-1
  df_y<-length(y)-1
  
  F_stat<-if(var_x>var_y)var_x/var_y else var_y/var_x
  df1<-if(var_x>var_y)df_x else df_y
  df2<-if(var_x>var_y)df_y else df_x
  
  p_value<-2*min(pf(F_stat,df1,df2,lower.tail=FALSE),
                 pf(F_stat,df1,df2,lower.tail=TRUE))
  
  cat("F_stat:",F_stat,"\n")
  cat("degrees_of_freedom:",df1,"and",df2,"\n")
  cat("p_value:",p_value,"\n")

  if(p_value<alpha){
    cat("conclusion:reject H0→variances are significantly different\n")
  }else{
    cat("conclusion:fail to reject H0→no significant difference in variances\n")
  }
}


# (b) Use the data sets below to carry out this test with α = 0.05.
# x = c(1067.7, 984.3,998.8,1025.9,1060.9,959.1,1013.8,
#       
#       1047.0,987.8,1051.0,885.2,
#       1049.5,1098.2,1001.5,1011.1,991.6)
# 
# y = c(957.6, 981.8, 1096.5, 984.4, 1074.3, 929.4, 1056.0,
#       
#       1012.3, 1040.7, 1099.5,
#       1006.1, 1064.3, 865.6, 944.4, 1091.8, 952.1)

x<-c(1067.7,984.3,998.8,1025.9,1060.9,959.1,1013.8,
     1047.0,987.8,1051.0,885.2,1049.5,1098.2,1001.5,1011.1,991.6)

y<-c(957.6,981.8,1096.5,984.4,1074.3,929.4,1056.0,
     1012.3,1040.7,1099.5,1006.1,1064.3,865.6,944.4,1091.8,952.1)

two_sample_variance_test(x,y,alpha=0.05)


# (5) Wilcoxon signed rank test for two dependent samples: This is carried out using wilcox.test()
# function in R again, and the parameters are already described above in the one sample
# tests. Carry out this test for the following data with conf.level=0.95 for the null hypothesis
# that the mean for the paired sample is greater than 0, i.e. the two samples have different means.
# Pre_therapy : 74, 72, 62, 58, 59, 65, 54, 63, 80, 66, 65, 64, 79, 60
# Post_therapy : 79, 55, 53, 53, 74, 55, 64, 55, 39, 44, 37, 68, 54, 54

pre_therapy<-c(74,72,62,58,59,65,54,63,80,66,65,64,79,60)
post_therapy<-c(79,55,53,53,74,55,64,55,39,44,37,68,54,54)

result<-wilcox.test(pre_therapy,post_therapy,alternative="greater",paired=TRUE,conf.level=0.95)
print(result)


# (6) Wilcoxon rank sum test for unpaired samples and Mann-Whitney test: Use the wilcox.test()
# function to carry out the Wilcoxon rank sum test for two independent samples given below
# with the alternate hypothesis that the placebo population has a smaller mean than
# that exposed to the drug. Use a confidence level of 95%.
# drug : 31.7,75.0,101.1,60.5,62.8,59.3,58.9,91.3,99.1,52.0,39.1
# placebo : 59.3,72.7,100.5,64.7,69.0,72.7,69.6,97.4,100.6,65.1,65.7


drug <- c(31.7, 75.0, 101.1, 60.5, 62.8, 59.3, 58.9, 91.3, 99.1, 52.0, 39.1)
placebo <- c(59.3, 72.7, 100.5, 64.7, 69.0, 72.7, 69.6, 97.4, 100.6, 65.1, 65.7)

test_result <- wilcox.test(placebo, drug, alternative = "less", conf.level = 0.95)
print(test_result)

if(test_result$p.value < 0.05) {
  cat("showing significant difference (p =", test_result$p.value, ") - placebo values are being lower than drug values\n")
} else {
  cat("showing no significant difference (p =", test_result$p.value, ") - placebo values are not being lower than drug values\n")
}


# (7) Kruskal Wallis test: In R, this test is performed by kruska.test() function.
# Group-1 : 220 214 203 184 186 200 165
# Group-2 : 262 193 225 200 164 266 179
# Group-3 : 272 192 190 208 231 235 141
# Group-4 : 190 255 247 278 230 269 289
# Reform the data above into a (x,y) form where x stands for the value and y is the
# category of the group (use rep() function to label each data point according to the
# group), then use the above R function with arguments x and y. Print the results output
# by the function.


group1<-c(220,214,203,184,186,200,165)
group2<-c(262,193,225,200,164,266,179)
group3<-c(272,192,190,208,231,235,141)
group4<-c(190,255,247,278,230,269,289)

x<-c(group1,group2,group3,group4)
y<-factor(rep(1:4,each=7),labels=c("Group1","Group2","Group3","Group4"))

result<-kruskal.test(x~y)
print(result)


# (8) Chi-square GoF test: Based on what we learnt in class, write a function to perform the
# GoF test based on input data of expected and observed values. We will use qchisq()
# function to get the critical value and pchisq() to get the p-value. Use the function to
# carry out the test for the following data:
#   Observed : 32, 82, 77, 49
# Expected : 40,80,80,40

chi_square_gof <- function(observed, expected, alpha = 0.05) {
  chi_stat <- sum((observed - expected)^2 / expected)
  df <- length(observed) - 1
  p_value <- pchisq(chi_stat, df, lower.tail = FALSE)
  critical_value <- qchisq(1 - alpha, df)
  decision <- ifelse(chi_stat > critical_value, "Reject H0", "Fail to reject H0")
  
  return(list(
    chi_square = chi_stat,
    degrees_of_freedom = df,
    p_value = p_value,
    critical_value = critical_value,
    decision = decision
  ))
}


observed <- c(32, 82, 77, 49)
expected <- c(40, 80, 80, 40)

result <- chi_square_gof(observed, expected)
result

#IV. One-way ANOVA (multiple sample tests)

# (1) ANOVA test on people on the Titanic ship
# (a) Read in the data file called titanic.csv. Make histogram plots of groups of
# people marked as ‘1st’, ‘2nd’ and ‘3rd’ (use about 30 bins) to check whether the
# three samples have approximately the same variance and are looking approximately
# normal. Then we are justified in using ANOVA. Make the plots in a 3x1 grid. Our
# null hypothesis is that the mean age is same among the 3 groups of people.

titanic<-read.csv("titanic.csv")

first_class<-titanic$Age[titanic$Pclass==1]
second_class<-titanic$Age[titanic$Pclass==2]
third_class<-titanic$Age[titanic$Pclass==3]

par(mfrow=c(3,1))

hist(first_class,breaks=30,main="1st class age distribution",xlab="age",col="lightblue")
hist(second_class,breaks=30,main="2nd class age distribution",xlab="age",col="lightgreen")
hist(third_class,breaks=30,main="3rd class age distribution",xlab="age",col="lightpink")

par(mfrow=c(1,1))
anova_result<-aov(Age~factor(Pclass),data=titanic)
summary(anova_result)

# (b) To quantitatively verify the equal variance assumption, we are going to determine
# the mean and standard deviations from each group. Load the package dplyr, we
# will use two functions group_by() and summarise(). Study the output of the
# following commands:
#   titanic_by_passenger_class<- group_by(titanicData,passenger_class)
# summarise(titanic_by_passenger_class, group_mean=mean(age,na.rm=TRUE),
#           group_sd=sd(age,na.rm=TRUE)
# What do you find? Are the standard deviations similar between the groups? Print
# a statement showing the conclusion of this comparison.

library(dplyr)
titanic_by_class<-group_by(titanic,Pclass)
class_stats<-summarise(titanic_by_class,
                       mean_age=mean(Age,na.rm=TRUE),
                       sd_age=sd(Age,na.rm=TRUE))
print(class_stats)
max_sd<-max(class_stats$sd_age)
min_sd<-min(class_stats$sd_age)
if(max_sd/min_sd<2){
  print("The standard deviations are similar between groups, supporting equal variance assumption")
}else{
  print("The standard deviations differ between groups,not supporting equal variance assumption")
}


# (c) We fit the ANOVA model to the data using lm() function. This function takes
# a formula and data frame as arguments. A model formula takes the form of a
# response variable followed by a tilde( ) and then at least one explanatory variable.
# Here we will give age~passenger_class which tells R to ‘fit’ a model in which
# age of passengers are grouped by the variable passenger_class. The command
# therefore is
# lmresults <- lm(age~passenger_class, data=titanicData)
# anova(lmresults)
# The anova() function returns the ANOVA table as output. What is your statistical
# inference/decision from the table, and therefore what is the statistical conclusion?

lmresults<-lm(Age~factor(Pclass),data=titanic)
anova_table<-anova(lmresults)
print(anova_table)

p_value<-anova_table$`Pr(>F)`[1]
if(p_value<0.05){
  print("Decision: Reject null hypothesis - Conclusion: There are significant differences in mean age between passenger classes")
}else{
  print("Decision: Fail to reject null hypothesis - Conclusion: No significant age differences between passenger classes")
}

# (d) The ANOVA tells us that at least one group has a mean different from the others,
# but does not tell us which group means are actually different. A Tukey-Kramer’s
# test tests the null hypothesis that there is no difference between the population
# means of all pairs of groups. This is invoked in R by using TukeyHSD() function.
# Execute the following command;
# TukeyHSD(aov(lmresults))
# Look at the columns labeled ‘diff’ and ‘p adj’. The p-values are calculated using a
# 95% confidence interval, and ‘lwr’ and ‘upr’ denote lower and upper bounds of the
# interval. From the output you should be able to see that the CIs do not include
# zero, and since the p-value is less than 0.05 in all the cases, the H0 is rejected for all
# pairs, and we will conclude that the means of the three populations are significantly
# different from each other.


tukey_results<-TukeyHSD(aov(lmresults))
print(tukey_results)

all_significant<-all(tukey_results$`factor(Pclass)`[,"p adj"]<0.05)
if(all_significant){
  print("All pairwise comparisons show significant differences (p<0.05):")
  print("1st-2nd, 1st-3rd, and 2nd-3rd class mean ages are all significantly different")
}else{
  significant_pairs<-rownames(tukey_results$`factor(Pclass)`)[tukey_results$`factor(Pclass)`[,"p adj"]<0.05]
  print(paste("Significant differences exist for:",paste(significant_pairs,collapse=", ")))
}

# (e) Let us also perform a Kruskal-Wallis test for the above data since the test does not
# need us to assume normal distribution like ANOVA. Execute
# kruskal.test(age~passenger,data=titanidData). Check whether the p value
# leads to the same conclusion as the parametric test above.

kruskal_result<-kruskal.test(Age~Pclass,data=titanic)
print(kruskal_result)

anova_p<-anova_table$`Pr(>F)`[1]
kw_p<-kruskal_result$p.value

if(kw_p<0.05){
  kw_conclusion<-"Kruskal-Wallis concludes significant differences exist"
}else{
  kw_conclusion<-"Kruskal-Wallis finds no significant differences"
}

if(sign(anova_p-0.05)==sign(kw_p-0.05)){
  comparison<-"matches the ANOVA conclusion"
}else{
  comparison<-"differs from the ANOVA conclusion"
}

print(paste(kw_conclusion,"(p=",round(kw_p,4),")",comparison))


# (2) Cuckoo egg size problem:

# (a) The European cuckoo does not look after its own eggs, but instead lays them in
# the nests of birds of other species. Previous studies showed that cuckoos sometimes
# have evolved to lay eggs that are colored similarly to the host bird species’ eggs.
# Is the same true of egg size – do cuckoos lay eggs similar in size to the size of the
# eggs of their hosts? The data file “cuckooeggs.csv” contains data on the lengths
# of cuckoo eggs laid in the nests of a variety of host species. Here we compare the
# mean size of cuckoo eggs found in the nests of different host species. Plot a multiple
# histogram showing cuckoo egg lengths by host species.



library(ggplot2)
cuckoo_data <- read.csv("/home/ibab/R/cuckooeggs.csv")

#creating histogram comparison of egg lengths by host species
ggplot(data = cuckoo_data, aes(x = egg_length, fill = host_species)) +
  geom_histogram(
    binwidth = 0.5,          
    position = "identity",   #allowing histogram bars to overlap
    alpha = 0.7,            
    color = "black",        
    linewidth = 0.3
  ) +
  labs(
    title = "Cuckoo Egg Lengths by Host Species",
    x = "Egg Length (mm)",
    y = "Count",
    fill = "Host Species"
  ) +
  theme_minimal() +         
  facet_wrap(~host_species)


# (b) Calculate a table that shows the mean and standard deviation of length of cuckoo
# eggs for each host species. Look at the graph and the table. For these data, would
# ANOVA be a valid method to test for differences between host species in the lengths
# of cuckoo eggs in their nests?


library(dplyr)

#calculating mean and standard deviation of egg lengths for each host species
summary_table<-cuckoo_data%>%
  group_by(host_species)%>%
  summarise(mean_length=mean(egg_length),
            sd_length=sd(egg_length))

print(summary_table)

#INTERPRETATION:-
#all groups show roughly bell-shaped distributions. No extreme outliers. 
#The difference is not that huge in standard deviation and we assume that each egg measurement is independent. 
#Hence ANOVA would be valid to test for differences in cuckoo egg lengths across host species.


#(c) Use ANOVA to test for a difference between host species in the mean size of the
#cuckoo eggs in their nests. What is your conclusion?


library(ggplot2)
cuckoo_data<-read.csv("/home/ibab/R/cuckooeggs.csv")

anova_result<-aov(egg_length~host_species,data=cuckoo_data)
summary(anova_result)

#CONCLUSION:-
#The p-value is much smaller than the significance level
#so we reject the null hypothesis

# (d) Assuming that ANOVA rejected the null hypotheses of no mean differences, use a
# Tukey-Kramer test to decide which pairs of host species are significantly different
# from each other in cuckoo egg mean length. What is your conclusion?


tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
plot(tukey_result)


#Conclusion:-
#Wren is significantly different from almost every other species in terms of egg length.
#Meadow Pipit vs Hedge Sparrow and Tree Pipit vs Meadow Pipit but most other species have similar egg sizes.


#(3) Maize and malaria problem:

# (a) The pollen of the maize (corn) plant is a source of food to larval mosquitoes of
# the species Anopheles arabiensis, the main vector of malaria in Ethiopia. The
# production of maize has increased substantially in certain areas of Ethiopia recently,
# and over the same time period, malaria has entered in to new areas where it was
# previously rare. This raises the question, is the increase of maize cultivation partly
# responsible for the increase in malaria?
# One line of evidence is to look for an association between maize production and
# malaria incidence at different geographically dispersed sites (Kebede et al. 2005).
# The data set “malaria vs maize.csv” contains information on several high-altitude
# sites in Ethiopia, with information about the level of cultivation of maize (low,
# medium or high in the variable maize yield) and the rate of malaria per 10,000
# people (incidence rate per ten thousand).
# Plot a multiple histogram to show the relationship between level of maize produc-
# tion and the incidence of malaria.


library(ggplot2)
malaria_data<-read.csv("/home/ibab/R/malaria vs maize.csv")

malaria_data$maize_yield<-factor(malaria_data$maize_yield,
                                 levels=c("low","medium","high"))

ggplot(malaria_data,aes(x=incidence_rate_per_ten_thousand,fill=maize_yield))+
  geom_histogram(position="dodge",bins=10,color="black")+
  facet_wrap(~maize_yield)+
  labs(title="Malaria incidence across maize yield levels",
       x="Malaria incidence per 10,000",
       y="Frequency")+
  theme_minimal()+
  scale_fill_brewer(palette="Set2")


# (b) ANOVA is a logical choice of method to test differences in the mean rate of malaria
# between sites differing in level of maize production. Calculate the standard deviation
# of the incidence rate for each level of maize yield. Do these data seem to
# conform to the assumptions of ANOVA? Describe any violations of assumptions
# you identify.

library(dplyr)

malaria_data %>%
  group_by(maize_yield) %>%
  summarise(sd_incidence = sd(incidence_rate_per_ten_thousand, na.rm = TRUE))

# Independence of observations:The data points should be independent of each other. 
# If the data involves clustered observations or repeated measures this assumption might be violated.
# ANOVA assumes that the residuals (the differences between observed and predicted values) are normally distributed. 


# (c) Compute the log of the incidence rate and redraw the multiple histograms for
# different levels of maize yield. Calculate the standard deviation of the log incidence
# rate for each level of maize yield. Does the log-transformed data better meet the
# assumptions of ANOVA than did the untransformed data?

malaria_data$log_incidence <- log10(malaria_data$incidence_rate+1)

#plotting log-transformed data
ggplot(malaria_data, aes(x=log_incidence, fill=maize_yield)) +
  geom_histogram(position="identity", alpha=0.6, bins=15) +
  labs(title="Log Malaria Incidence by Maize Yield Level",
       x="Log10(Malaria Incidence +1)",
       y="Frequency",
       fill="Maize Yield") +
  facet_wrap(~maize_yield) +
  theme_minimal()

#calculating stats for log-transformed data
log_stats <- malaria_data %>%
  group_by(maize_yield) %>%
  summarise(
    mean_log=mean(log_incidence),
    sd_log=sd(log_incidence),
    n=n()
  )
print(log_stats)


#(d) Test for an association between maize yield and malaria incidence.
malaria_data <- malaria_data[!is.na(malaria_data$maize_yield), ]
anova_orig <- aov(incidence_rate_per_ten_thousand~maize_yield, data=malaria_data)
summary(anova_orig)

anova_log <- aov(log_incidence~maize_yield, data=malaria_data)
summary(anova_log)

kruskal.test(incidence_rate~maize_yield, data=malaria_data)



#(4) Circadian rhythms of diseased animals:

# (a) Animals that are infected with a pathogen often have disturbed circadian rhythms.
# (A circadian rhythm is an endogenous daily cycle in a behavior or physiological
# trait that persists in the absence of time cues.) Shirasu-Hiza et al. (2007) wanted
# to know whether it was possible that the circadian timing mechanism itself could
# have an effect on disease. To test this idea they sampled from three groups of fruit
# flies: one “normal”, one with a mutation in the timing gene tim01, and one group
# that had the tim01 mutant in a heterozygous state. They exposed these flies to
# a dangerous bacteria, Streptococcus pneumoniae, and measured how long the flies
# lived afterwards, in days. The date file “circadian mutant health.csv” shows some
# of their data.
# Plot a histogram of each of the three groups. Do these data match the assumptions
# of an ANOVA?
# Load necessary libraries

circadian_data <- read.csv("/home/ibab/R/circadian mutant health.csv")

head(circadian_data)
par(mfrow = c(3, 1))
hist(circadian_data$days_to_death[circadian_data$genotype == "tim01 (rescued)"],
     breaks = 5, main = "Normal Flies", 
     xlab = "Days to Death", col = "lightgreen")

hist(circadian_data$days_to_death[circadian_data$genotype == "tim01"],
     breaks = 5, main = "tim01 Mutant Flies", 
     xlab = "Days to Death", col = "lightblue")

hist(circadian_data$days_to_death[circadian_data$genotype == "wild type"],
     breaks = 5, main = "Heterozygous Flies", 
     xlab = "Days to Death", col = "salmon")


#(b) Use a Kruskal-Wallis test to ask whether lifespan differs between the three groups
#of flies.
perform_kruskal_test <- function(data) {
  kruskal_test_result <- kruskal.test(days_to_death ~ genotype, data = data)
  print(kruskal_test_result)
  p_val <- kruskal_test_result$p.value
  print("[Kruskal-Wallis Test Conclusion]:")
  if (p_val < 0.05) {
    print(paste("The p-value is", round(p_val, 4), "< 0.05, so we reject H0."))
    print("There is a significant difference in lifespan between the groups of flies.")
  } else {
    print(paste("The p-value is", round(p_val, 4), ">= 0.05, so we fail to reject H0."))
    print("There is no significant difference in lifespan between the groups of flies.")
  }
}
perform_kruskal_test(circadian_data)

