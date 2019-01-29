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
