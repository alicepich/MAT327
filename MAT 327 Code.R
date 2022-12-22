histogram <- function(model) {
  hist(model$weight,
       xlab   = "Car Model",
       main   = "Histogram of Model/Weight",
       breaks = 12,
       col    = "dodgerblue",
       border = "darkorange")
}

#The code below will define 3 variables
carweight <- X35_Car_Data$WEIGHT
carmodel <- X35_Car_Data$MODEL
carlength <- X35_Car_Data$LENGTH

plot(carlength, carweight, xlab = "Car Length", ylab = "Car Weight", main = "Comparison of Car Length vs Car Length")

#The code below will plot a histogram of the car weights

hist(carweight)

#The code below will plot the same histogram above with the x and y labels added
hist(carweight, xlab= "Weight in Pounds", ylab = "Number of Cars in the Dataset", main = "Car Weight Histogram")

#The code below will plot a scatterplot of the car length and the car weight
plot(carlength, carweight)


#The code below will define two variables
carbraking <- X35_Car_Data$BRAKING
carcylinders <- X35_Car_Data$CYLINDERS

#The code below will output the mean car weight
mean(carweight)

#The mean carweight is 3659.042

#The code below will calculate the standard deviation for the car weight
sd(carweight)

#The standard deviation of the carweight is 646.8724


#The code below will output the variance of the car weight
var(carweight)

#The variance of the car weight is 418443.9

#The code below will provide the 5 number summary of the car weight Column, that is the minimun, 1st, 2nd, and 3rd quartiles, and the maximum

fivenum(carweight, na.rm = TRUE)

#In order, the 5 number summary of the car weight column is:
# minimum = 2468.0
# 1st quartile = 3127.5
# 2nd quartile (median) = 3626.0
# 3rd quartile = 4236.0
# Maximum = 4949.0

#The Code below will plot a box-whisker plot of the 5 number summary computed above
boxplot(carweight, horizontal = TRUE, col = "orange", ylab ="Cars", xlab = "Weight in Pounds", main = "Box-Whisker Plot of 5 Number Summary for Car Weights")

#Repeat computations above for car breaking, car cylinders, car length, and engine displacement

#The code below will compute a histogram for car braking
hist(carbraking, main = "Histogram of Car Braking Distance", xlab = "Braking Distance in Feet")

#The code below will compute a histogram for car length
hist(carlength, main = "Histogram of Car Length", xlab = "Length in Inches")


#Compute the residuals and plot


#The code below will plot the distance needed to break versus the weight of a car
plot(carbraking ~ carweight, main = "This Scatterplot Compares the Amount of Feet required for a Vehicle to Break When Compared to the Car's Weight", xlab= "Car Wight in Pounds", ylab = "Breaking Distance in Feet")

#The code below will calculate the correlation between the distance needed to break and a car's weight
cor(carbraking,carweight)

#The correlation between a car breaking distance and a car's weight is 0.1419226 which suggest that there is no strong correlation since this umber is close to zero

#The code below will compute the 95th percentile confidence interval for car weights

qnorm(0.95, mean(carweight), sd(carweight))

#The 95th percentile confidence interval for Car Weights is 4723.052

#The Confidence interval is given by:
t <- qt(0.975, mean(carweight)-1)
t

#The confidence interval is 1.960613

#The code below will compute the error amount
Erroramount = t*sd(carweight)/sqrt(mean(carweight))


#The Error amount is 20.96654

#We have that the confidence interval for car weights is 3659.042 +- 20.96654

#The code below will plot a Histogram of the Car Lengths
hist(carlength, main = "Histogram of Car Lengths", xlab = "Car Length in Feet", ylab = "Number of Cars with This length")

#The code below will compute a simple linear regression of the carweight and car braking with the car braking being the predictor variable
weight.lm <- lm(carweight ~ carbraking, data= X35_Car_Data)
weight.lm

#We obtain the following coefficients: Y intercept is 974.55 and the slope is 20.29

#The code below will output the residuals for the linear regression model
resid(weight.lm)

#The code below will output a histogram of the residuals
hist(resid(weight.lm), main = "Residuals of carweight and car breaking", xlab = "residual value", ylab = "Frequency of Residuals")

#The following code will print a linear regression model usin car braking, carcylinders, and car length as the predictor variables
lm2 <- lm(carweight ~ carbraking+carcylinders+carlength, data = X35_Car_Data)
lm2
#For the intercept we obtain -3154.481, and for coefficients we have -2.462, 216.645, and 32.151

#The code below will create a subset of the data points
data <- c(3109, 2870, 2915, 2985, 2563, 3009, 4253, 4006, 4230, 2844, 3217, 3417, 3549, 3962, 3891, 4230)

s<-sd(data)
s
n <- 16
t <- qt(0.995,16-1)
xbar <-mean(data)
L <- xbar - t*s/sqrt(n)
U = xbar + t*s/sqrt(n)
z <- qnorm(0.95)
z
xbar - z*s/sqrt(n)
xbar + z*s/sqrt(n)

