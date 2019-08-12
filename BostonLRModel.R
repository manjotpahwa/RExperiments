# First intro to R and ML

library(MASS)
library(ISLR)
ls()
names(Boston)
?Boston
plot(medv~lstat, Boston)
# Simple LM with median value versus status
fitLstat = lm(medv~lstat, data = Boston)
fitLstat
head(fitLstat)
summary(fitLstat)
abline(fitLstat, col="red")
names(fitLstat)
confint(fitLstat)
predict(fitLstat, data.frame(lstat=c(5, 10, 15)), interval = "confidence")

# Simple LM with median value versus status and age of house
fitAgeLstat = lm(medv~lstat+age, data = Boston)
fitAgeLstat
abline(fitAgeLstat, col="blue")

fitAll = lm(medv~., data = Boston)
summary(fitAll)
abline(fitAll, col="green")
plot(fitAll)

# Updating our LM to take into account all variables
fitUpdate = update(fitAll, ~.-age-indus)
summary(fitUpdate)

# Simple LM with interactions between the status and age
fitInteract = lm(medv~lstat*age, Boston)
summary(fitInteract)

# Quadratic LM with median value versus status
fitQuad = lm(medv~lstat+I(lstat^2), Boston)
summary(fitQuad)
attach(Boston)
par(mfrow=c(1, 1))
plot(medv~lstat)
points(lstat, fitted(fitQuad), col="yellow", pch=20)

fitPoly = lm(medv~poly(lstat, 4))
points(lstat, fitted(fitPoly), col="blue", pch=20)

# Simple and quadratic LM with median value versus crime rate
#fitCrime = lm(medv~crim, data = Boston)
fitCrime = lm(medv~crim+I(crim^2), data = Boston)
fitCrime
plot(medv~crim)
#abline(fitCrime, col="orange")
points(crim, fitted(fitCrime), col="orange", pch=20)

# Simple LM with median value versus crime rate and ratio of blacks
fitCrimeBlack = lm(medv~crim+black, data = Boston)
fitCrimeBlack
plot(medv~crim+black)
abline(fitCrimeBlack, col="red")

# Simple LM for our carseats data
summary(Carseats)
fitCar = lm(Sales~.+Income:Advertising+Age:Price, Carseats)
summary(fitCar)
