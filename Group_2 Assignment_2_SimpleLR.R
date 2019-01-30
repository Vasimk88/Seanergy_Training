data()
data(airquality)
names(airquality)
attach(airquality)

model2=lm(Ozone~Solar.R+Wind+Temp,data=airquality)

model2

summary(model2)

#Null Hypothesis = Only Solar.R and Intercept value is required to predict the ozone value

#Alternate Hypothesis = Only Solar.R and Intercept value are not enough to predict the ozone value

#We build a linear model using the additional parameters Wind and Temp which are independent variables

#On using the additional parameters we got the linear equation as 
#Ozone = -64.34 + 0.05(Solar.R) -3.33(Wind) + 1.65(Temp)

#On computing the summary of the above model we got p value for Intercept = 0.006, Solar.R = 0.01
#Wind = 0.00000152 and Temp = 0.00000000242

#On comparing the p value for every additional value with standard condition to reject the null hypothesis p < 0.05

#We have got p value for model = 2.2e-16 which is very much less than 0.05

#Hence we can reject the null hypothesis stating Only Solar.R value is enough to predict the ozone value

#Coeff. of determination value for the model = 59.48% which is greater than 11.33 for previous model
#which we have used only Solar.R and dependent variable to calculate Ozone. But this value 59.48% is still
#less than 70% hence the model is not much reliable to predict the ozone value with more accuracy.













