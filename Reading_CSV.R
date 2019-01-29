# Read CSV into R
filepath="C:/Driver Efficiency/CSV-PayrollHours.csv"
MyData <- read.csv(file=filepath, header=TRUE, sep=",")
#Attaching the file
attach(MyData)
#Calculating Mean of a Column
mean(MyData$Hours)
