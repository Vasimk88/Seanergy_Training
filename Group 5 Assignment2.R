data()
data(airquality)
names(airquality)
attach(airquality)
plot(Ozone~Solar.R)
plot(Ozone~Solar.R,data=airquality)

#Trying the Mean of Ozone
#mean(airquality$Ozone)
#airquality$Ozone

#calculate mean ozone concentration (na´s removed)
mean.Ozone=mean(airquality$Ozone,na.rm=T)

abline(h=mean.Ozone)

#use lm to fit a regression line through these data:

model1=lm(Ozone~Solar.R,data=airquality)

model1


predict(model1,data.frame(Solar.R=12))

abline(model1,col="red")
plot(model1)

termplot(model1)
summary(model1)


#Second Model with Solar Wind and Temp data

model2=lm(Ozone~Solar.R+Wind+Temp,data=airquality)
model2

abline(model2,col="Blue")
plot(model2)

termplot(model2)
summary(model2)














##  Assumptions of linear regression

# 1. The mean of residuals is zero
airquality

#mod <- lm(dist ~ speed, data=cars)
mean(model2$residuals)

# Assumption 3: Homoscedasticity of residuals or equal variance

par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
#mod_1 <- lm(mpg ~ disp, data=mtcars)  # linear model
#plot(mod_1)

#In this case, there is a definite pattern noticed.
#So, there is heteroscedasticity. Lets check this on a different model.

#mod <- lm(dist ~ speed, data=cars[1:20, ])  #  linear model
plot(model2)

#Now, the points appear random and the line looks pretty flat, with no increasing or decreasing trend.
#So, the condition of homoscedasticity can be accepted.

# Assumption 4: The X variables and residuals are uncorrelated

##Do a correlation test on the X variable and the residuals.

#mod.lm <- lm(dist ~ speed, data=cars)
cor.test(model1$residuals, model2$residuals)  # do correlation test 


#p-value is high, so null hypothesis that true correlation is 0 can't be rejected.
#So, the assumption holds true for this model.

# Assumption 5: The number of observations must be greater than number of Xs

##This can be directly observed by looking at the data.

# Assumption 6: The variability in X values is positive

##This means the X values in a given sample must not all be the same (or even nearly the same).

var(airquality$Temp)  

# The variance in the X variable above is much larger than 0. So, this assumption is satisfied.

# Assumption 8: The regression model is correctly specified

##This means that if the Y and X variable has an inverse relationship,
##the model equation should be specified appropriately:


## Y=??1+??2???(1/X)

# Assumption 8: No perfect multicollinearity

## If the VIF of a variable is high, it means the information in that variable is already
##explained by other
## X variables present in the given model, which means, more redundant is that variable.

# VIF=1/(1???Rsq)

## where, Rsq is the Rsq term for the model with given X as
## response against all other Xs that went into the model as predictors.













