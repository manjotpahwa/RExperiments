require(ISLR)
require(MASS)

lda.fit = lda(Direction~Lag1+Lag2, data = Smarket, subset = Year < 2007)
lda.fit
plot(lda.fit)
Smarket.2005 = subset(Smarket, Year < 2007)
lda.pred = predict(lda.fit, Smarket.2005)
# this is only to check what type of variable lda.pred is since we could not use it as a matrix.
class(lda.pred)
# use the list as we had indexed earlier.
data.frame(lda.pred)[1:5,]
# Class is classification of lda.pred which is the prediction. Here we plot it against true value.
# Confusion matrix
table(lda.pred$class, Smarket.2005$Direction)
# Calculate mean of our accuracy
mean(lda.pred$class == Smarket.2005$Direction)
