#PART B - B2

chirps <-c(14.7, 19.8, 18.4, 17.1, 15.5, 19.5)
temp <-c(69.7, 93.3, 84.3, 80.6, 75.2, 90.0)

model <- lm(temp ~ chirps)

slope <- coef(model)[1]
intercept <- coef(model)[2]

plot(chirps, temp, pch = 16, col = "navy", main = "Temperature vs Chirps",
     xlab = "Chirps", ylab = "Temperature")
abline(model, col = "hotpink4", lwd = 2)

legend("topleft",legend=c(paste("Slope:",round(slope,2)),
                             paste("Intercept:",round(intercept, 2))),
       bty="n")
legend("bottomright", legend=paste("y =",round(coefficients[2],2), "* x +",round(coefficients[1],2)), col="hotpink4", lwd=2)

