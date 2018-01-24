# To start setwd("~/Desktop/OHSU/Fibrin Strucutre Paper/MicroplateData")

# Required function for error bars
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
      if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
            stop("vectors must be same length")
      arrows(x,y+upper, x, y-lower, angle=90, code=2, length=length, ...)
}
# Must install xlsx if not installed : install.packages("xlsx")
# Must install matrixStats if not installed : install.packages("matrixStats")
library(xlsx)
library(matrixStats)

#Load all the Xcel sheets FF is fibrin formation and F-scan is scan 
# from 400-800 nm for fiber thickness
F_scan <- as.matrix(read.xlsx("20160630.xlsx", sheetIndex=1, header = TRUE))
FF_1 <- as.matrix(read.xlsx("20160630.xlsx", sheetIndex=2, header = TRUE))

# Want to subtract the backgroun from all the images
# First step is to find the minimum background in each of the columns
# Next perform a sweep of the region to subtract the background (subtraction is
# default operation)
bgrd <- apply(FF_1[3:nrow(FF_1),],1,min) # Finds the minimum in each row
FF_1[3:nrow(FF_1),] <- sweep(FF_1[3:nrow(FF_1),],1,min(bgrd)) #This subtracts out first point of array

# Start with fibrin formation
Time <- as.numeric(FF_1[1,])

# Condition means and standard deviations


#Put all of the data sets together, each condition is a column over time
All <- t(FF_1)
All <- All[,3:ncol(All)]

# This loop will calculate slope and maximum turbidity for each condition
# This loop works by calculating the discrete slope throughout the curves and returning the maximum gradient
# All_names <- names(All)
max_slope <- as.numeric(vector(length = ncol(All)))
for (i in 1:ncol(All)) {
       x <- Time
      y <- All[,i]
      grad <- vector(length = length(y))
      for (j in 2:length(y)) {
            grad[j] <- (y[j]-y[j-1])/(x[j]-x[j-1])
            k <- max(grad)
      }
      
      max_slope[i] <- k
}
max_slope

write.xlsx(lag_time, "data.xlsx")

# To determine the lag time with linear interpolation
lag_time <- as.numeric(vector(length = ncol(All)))
for (i in 1:ncol(All)) {
      x <- Time
      y <- All[,i]
      lag_y <- max(y)*0.1
      j <- min(which(y > lag_y)) #creates an index of where y is first greater than 0.1*max
      # Next use this position and interpolate with x and y before to find the time
      if (j == 1){
            lag_time[i] <- x[j]
             } else {
      lag_time[i] <- x[j-1] + (y[j]-lag_y)*(x[j]-x[j-1])/(y[j]-y[j-1])
}
}
lag_time

# To obtain the maxes and lag time
max <- apply(All,2,max)
Lag <- max*0.1 #Where does the clot reach 10% of max, this is the lag time

# Plotting all of the data
png(filename = "20160515plots.png")
par(mfrow=c(2,2))

# PolyP
plot(Time, CTL_PP_mean, type="l", ylab = "Absorbance", xlab = "", ylim=c(0,1.2), xlim=c(1,6000))
#error.bar(Time,PP_none_mean, PP_none_sd)
lines(Time,A6_PP_mean,type="l",col="red")
#error.bar(Time,PP_Ellagic_mean, PP_none_sd, col="red")
lines(Time,E11_PP_mean,type="l",col="blue")
#error.bar(Time,PP_PP0.01_mean, PP_none_sd, col="blue")
lines(Time,C9_PP_mean,type="l",col="springgreen4")
#error.bar(Time,PP_PP0.1_mean, PP_none_sd, col="springgreen4")
lines(Time,CTI_PP_mean,type="l",col="azure4")
#error.bar(Time,PP_PP0.1_mean, PP_none_sd, col="azure4")
legend("topleft", pch = "-", col = c("black","red", "blue", "springgreen4","azure4"), legend = c("CTL","1A6","14E11","10C9","CTI"))

# none
plot(Time, CTL_none_mean, type="l", ylab = "Absorbance", xlab = "", ylim=c(0,1.2), xlim=c(1,6000))
#error.bar(Time,none_none_mean, none_none_sd)
lines(Time,A6_none_mean,type="l",col="red")
#error.bar(Time,none_Ellagic_mean, none_none_sd, col="red")
lines(Time,E11_none_mean,type="l",col="blue")
#error.bar(Time,none_none0.01_mean, none_none_sd, col="blue")
lines(Time,C9_none_mean,type="l",col="springgreen4")
#error.bar(Time,none_none0.1_mean, none_none_sd, col="springgreen4")
lines(Time,CTI_none_mean,type="l",col="azure4")
#error.bar(Time,none_none0.1_mean, none_none_sd, col="azure4")
legend("topleft", pch = "-", col = c("black","red", "blue", "springgreen4","azure4"), legend = c("CTL","1A6","14E11","10C9","CTI"))


dev.off() # close file device


# PLOT THE SCANNED VARIABLES TO GET THE MASS TO LENGTH RATIO

# Wavelengths
W <- as.numeric(F_scan[1,])

# Condition means
# Plasma treatment_surface treatment_mean
# TF high
All <- t(F_scan)
All <- All[,2:ncol(All)]
# Want to get the y intercept so just want the first five values which are linear 
# the linear portion

ratio <- as.numeric(vector(length=ncol(All)))
for (i in 1:ncol(All)) {
      
      x <- 1/W[25:41]^2
      y <- 1/(W[25:41]^2*All[25:41,i])
      result <- lm (formula = y ~ x)
      ratio[i] <- 1/result$coefficients[1]
      
}
ratio

barplot(ratio_norm[3:14])
# plot to confirm that portion is linear
plot(x, y, type="l", ylab = "Absorbance", xlab = "")
abline(result)
legend("topleft", pch = "-", col = c("black","blue", "red", "springgreen4"), legend = c("Hdn_none","Hdn_Ellagic","Hdn_0.01", "Hdn_0.1"))
