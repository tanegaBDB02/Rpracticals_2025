#1
#reading the College.csv file into R
clg<- read.csv("/home/ibab/Tanega_Sarkar_IA2/College.csv")

#printing the column headings
colnames(clg) 

#listing the row names
rownames(clg)  

#2 
#Qualitative and Quantitative Variables
print("Private")#the only categorical variable

print(setdiff(colnames(clg),"Private"))  #others are numeric

#3
#statistical summary of the variables
print(summary(clg))

#summary is being printed and the mean is also being printed
#printing the standard deviation as it is not being printed
quantitative_vars <-clg[ , sapply(clg, is.numeric)]
means <- colMeans(quantitative_vars)
sds <- apply(quantitative_vars, 2, sd)
print(sds)


#4
#boxplot for outstate and private
boxplot(clg$Outstate[clg$Private == "Yes"],clg$Outstate[clg$Private == "No"],
        names = c("Yes", "No"),main = "Outstate vs Private",
        xlab = "Private",ylab = "Outstate",
        col = c("lightblue", "lightgreen"))

#5
#create a new qualitative variable Elite based on Top10perc
clg$Elite <- NA

#appling the condition — if Top10perc > 50 assign yes
clg$Elite[clg$Top10perc > 50] <- "yes"

#else assigning no to the rest
clg$Elite[clg$Top10perc <= 50] <- "no"

#converting the Elite column to a factor (categorical variable)
clg$Elite <- as.factor(clg$Elite)

#printing the count of elite and non-elite universities in a table form
table(clg$Elite)


#6
par(mfrow = c(1, 2))  

#choosing any two quantitative variables - Outstate and Room.Board

#histograms for Outstate with two bin types
hist(clg$Outstate,
     breaks = 10, col ='lightpink', main = "Outstate (10 bins)",xlab = "Outstate")

hist(clg$Outstate,
     breaks = 30,col ="darkred", add = TRUE)        
legend("topright", legend = c("10 bins", "30 bins"),
       fill = c("lightpink","darkred"))

#histograms for Room.Board with two bin choices
hist(clg$Room.Board,
     breaks = 10, col="lightblue",
     main = "Room.Board(10 bins)",
     xlab = "Room.Board")

hist(clg$Room.Board,
     breaks = 30,       
     col = "maroon", add=TRUE )          
legend("topright", legend = c("10 bins", "30 bins"),
       fill = c("lightblue", "maroon"))


#7
hist(clg$Apps, 
     breaks = 30, col = "lightblue", main = "Apps", xlab = "num of apps")

#mean and standard deviation
apps_mean <- mean(clg$Apps)
apps_sd <- sd(clg$Apps)

#scaling the normal curve to match histogram counts
x_vals <- seq(min(clg$Apps), max(clg$Apps), length=100)
y_vals <- dnorm(x_vals, mean = apps_mean, sd = apps_sd)
y_scaled <- y_vals * length(clg$Apps) * diff(hist(clg$Apps, breaks = 30, plot = FALSE)$breaks)[1]

#overlay the scaled curve
lines(x_vals, y_scaled, col = "purple", lwd = 2)

#legend
legend("topright",
       legend = c("Normal Distribution",
                  paste("Mean =", round(apps_mean,2)),
                  paste("SD =", round(apps_sd,2))),
       col = "purple",
       lwd = 2)


