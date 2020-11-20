### Nettle modelling and visualising in R tutorial ####
#loaded to Github repository 
#url = 


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

### simple plots of the data ####
plot(weight$Weight~weight$Height)
ggplot(data = weight, aes(x= Height, y = Weight)) + geom_point() + theme_bw() 
ggplot(data = weight, aes(x= Sex, y = Weight)) + geom_boxplot() + theme_bw() 

### linear model to look at the effect of sex on weight ####

model1 <- lm(Weight ~ Sex, data = weight)
summary(model1)

#relevel the sex variable so that men are first in the equation 
weight$Sex2=relevel(weight$Sex, ref="Male")

#re run the model with the same variables but male should come out as the intercept
model2 <- lm(Weight~Sex2, data = weight)
summary(model2)

