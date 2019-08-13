# Credit card default experiments. 
# Using linear regression to solve classification problems. 
# Since this is a case of binary classification (default/no default) we use logistic regression.
# For multiple possibilities, we use multiclass regression or discriminant analysis.
rm(list = ls())
ls()
require(ISLR)
names(Smarket)
summary(Smarket)
?Smarket
pairs(Smarket, col=Smarket$Direction)
# Logistic regression model, binomial tells it to fit a logistic regression model instead of the other ones possible.
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, type = "response")
glm.probs[1:5]
glm.pred = ifelse(glm.probs > 0.5, "Up", "Down")
attach(Smarket)
table(glm.pred, Direction)
# This shows just slightly better than chance. This is because of overfitting.
mean(glm.pred == Direction)

# Now we divide our data into training and test to get better results.
train = Year < 2005
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fit, newdata = Smarket[!train], type = "response")
glm.pred = ifelse(glm.probs > 0.5, "Up", "Down")
Direction.2005 = Smarket$Direction[!train]
table(glm.pred)
mean(glm.pred == Direction.2005)
summary(glm.fit)
