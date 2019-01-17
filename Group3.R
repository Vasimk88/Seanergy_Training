install.packages("ggplot2")
library(ggplot2)
library(scales)

# Read CSV into R
filepath="D:/ML/Machine Learning/Datasets/ProductionUnits.csv"


DsProductionUnits <- read.csv(file=filepath, header=TRUE, sep=",")


# graph by VehicleMiles:
ggplot(data = DsProductionUnits,
       aes(Day, VehicleMiles)) +
  stat_summary(fun.y = sum, geom = "bar") 




# graph by RevenueMiles:
ggplot(data = DsProductionUnits,
       aes(Day, RevenueMiles)) +
  stat_summary(fun.y = sum, geom = "bar") 



getmode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


VehicleMiles_Mean = mean(DsProductionUnits$VehicleMiles, trim=1/20)
VehicleMiles_Median = median(DsProductionUnits$VehicleMiles)
VehicleMiles_Mode = getmode(DsProductionUnits$VehicleMiles)
VehicleMiles_IQR = IQR(DsProductionUnits$VehicleMiles)







RevenueMiles_Mean = mean(DsProductionUnits$RevenueMiles, trim=1/20)
RevenueMiles_Median = median(DsProductionUnits$RevenueMiles)
RevenueMiles_Mode = getmode(DsProductionUnits$RevenueMiles)
RevenueMiles_IQR = IQR(DsProductionUnits$RevenueMiles)





VehicleMilesPolygon <- ggplot(DsProductionUnits, aes(x = Day))

VehicleMilesPolygon + geom_density() +
  geom_vline(aes(xintercept = mean(VehicleMiles)), 
             linetype = "dashed", size = 0.6)



RevenueMilesPolygon <- ggplot(DsProductionUnits, aes(x = Day))

RevenueMilesPolygon + geom_density() +
  geom_vline(aes(xintercept = mean(RevenueMiles)), 
             linetype = "dashed", size = 0.6)










