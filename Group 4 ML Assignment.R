#reading CSV file and getting data from Production Units

filepath="C:\\Users\\satya\\Downloads\\Production_Units.csv"
ProdUnitsData <- read.csv(file=filepath, header=TRUE, sep=",")
#Attaching the file
attach(ProdUnitsData)

data <- c(ProdUnitsData)

##Vehicle Hours Calculations

#Arithmetic mean

mean(ProdUnitsData$VehicleHours)

#Trimmed Meam

mean(ProdUnitsData$VehicleMiles, trim = 0.15)

#Median

median(ProdUnitsData$VehicleHours)

#Mode function

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Calculating Mode

Mode(ProdUnitsData$VehicleHours)

#Inter-Quartile Range

IQR(ProdUnitsData$VehicleHours)

#Histogram and frequency Polygon for Vehicle Hours

vehhours = hist(ProdUnitsData$VehicleHours)
lines(c(min(vehhours$breaks),vehhours$mids,max(vehhours$breaks)),c(0,vehhours$counts,0),type="l")

#Revenue Hours Calculations

#Mean

mean(ProdUnitsData$RevenueHours)

#Median
median(ProdUnitsData$RevenueHours)

#Mode

Mode(ProdUnitsData$RevenueHours)

#Inter-Quartile Range

IQR(ProdUnitsData$RevenueHours)

#Histogram and frequency Polygon for Revenue Hours

revhours = hist(ProdUnitsData$RevenueHours)
lines(c(min(revhours$breaks),revhours$mids,max(revhours$breaks)),c(0,revhours$counts,0),type="l")

# Both the distributions are skewed right. Most of the data containa zeros thus the median and mode are also zero.




#reading CSV file and getting data from Payroll Hours

filepath="C:\\Users\\satya\\Downloads\\Payroll_Hours.csv"
PayHoursData <- read.csv(file=filepath, header=TRUE, sep=",")
#Attaching the file
attach(PayHoursData)

PHdata= c(PayHoursData)

##Vehicle Hours Calculations

#Arithmetic mean

mean(PayHoursData$Hours)

#Trimmed Meam

mean(PayHoursData$Hours, trim = 0.15)

#Median

median(PayHoursData$Hours)

#Mode

Mode(PayHoursData$Hours)

#Inter-Quartile Range

IQR(PayHoursData$Hours)

#Histogram and frequency Polygon for Revenue Hours

payhours = hist(PayHoursData$Hours)
lines(c(min(payhours$breaks),payhours$mids,max(payhours$breaks)),c(0,payhours$counts,0),type="l")




