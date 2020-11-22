### Nettle modelling and visualising in R tutorial ####
#loaded to Github repository 
#url = 


#SESSIONS 1 AND 2
### load packages ####
library(readr)
library(ggplot2)
library(psych)
### load first data set ####
weight <- read.csv("Fun with R/weight.data.csv")
head(weight)
str(weight)
#gives means and sd of variables
describe(weight)
describeBy(weight$Weight, weight$Sex)

#mean centering data 
weight$Height.centred = weight$Height - mean(weight$Height)

### simple plots of the data ####
plot(weight$Weight~weight$Height)
(weightheight <- ggplot(data = weight, aes(x= Height, y = Weight)) + geom_point() + theme_bw())
(weightsex <- ggplot(data = weight, aes(x= Sex, y = Weight)) + geom_boxplot() + theme_bw())

### linear model to look at the effect of sex on weight ####

model1 <- lm(Weight ~ Sex, data = weight)
summary(model1)

#relevel the sex variable so that men are first in the equation 
weight$Sex2=relevel(weight$Sex, ref="Male")

#re run the model with the same variables but male should come out as the intercept
model2 <- lm(Weight~Sex2, data = weight)
summary(model2)

#model using continuous variable as the predictor 
#using height to predict weight
model3 <- lm(Weight ~ Height, data = weight)
summary(model3)

#add lm line on to previous graph
(lmweightheight <- ggplot(data = weight, aes(x= Height, y = Weight)) + geom_point() + theme_bw() + geom_smooth(method = 'lm', data = model3))

#multivariate analysis 
#how does weight respond to both sex and height
#height is mean centered
#How much more does a man weigh than a woman on average when the man and the woman in question are the same height?
model4 <- lm(Weight ~ Sex + Height.centred, data = weight)
summary(model4)

#check distribution of residuals 
#residuals should generally be normally distributed 
hist(model1$residuals)
hist(model2$residuals)
hist(model3$residuals)
hist(model4$residuals)

#for categorical variables the variance should be similar for all categories = homoscedaticity 
#if these residuals are not similar in variance you get heteroscedascity
plot(model4$residuals ~ weight$Sex)

#for continuous variables you can plot the residuals againt the fitted values
#fitted values = values of the linear predictor
#residuals = differences between the fitted values and the actual values
#should be an absence of overall pattern
#hopegully equally variabel around 0
plot(model3$residuals~model3$fitted.values)




#SESSION 3

### Load Data set ####
stars <- read.csv("Fun with R/star.data.csv")
head(stars)
summary(stars)
str(stars)
