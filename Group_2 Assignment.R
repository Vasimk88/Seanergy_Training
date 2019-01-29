filepath="D:/Production Units - Production Units.csv"
MyData <- read.csv(file=filepath, header=TRUE, sep=",")
attach(MyData)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#Vehicle Miles
col_M_mean_ten_Percent = mean(MyData$VehicleMiles, trim=1/10)
col_M_median = median(MyData$VehicleMiles)
col_M_Mode = getmode(MyData$VehicleMiles)
col_M_IQR = IQR(MyData$VehicleMiles)
hist(MyData$VehicleMiles, xlab="Vehicle Miles", col="Yellow", border = "Blue")
h_M <- hist(MyData$VehicleMiles, xlab="Vehicle Miles", col="Yellow", border = "Blue")
mp_M = c(min(h_M$mids) - (h_M$mids[2] - h_M$mids[1]), h_M$mids, max(h_M$mids) + (h_M$mids[2] - h_M$mids[1]))
freq_M = c(0, h_M$counts, 0)
lines(mp_M, freq_M, type = "b", pch = 20, col = "red", lwd = 3)



#Revenue Miles
col_N_mean_ten_Percent = mean(MyData$RevenueMiles, trim=1/10)
col_N_median = median(MyData$RevenueMiles)
col_N_Mode = getmode(MyData$RevenueMiles)
col_N_IQR = IQR(MyData$RevenueMiles)
hist(MyData$RevenueMiles, xlab="Revenue Miles", col="dark green", border = "Blue")
h_N <- hist(MyData$VehicleMiles, xlab="Vehicle Miles", col="dark green", border = "Blue")
mp_N = c(min(h_N$mids) - (h_N$mids[2] - h_N$mids[1]), h_N$mids, max(h_N$mids) + (h_N$mids[2] - h_N$mids[1]))
freq_N = c(0, h_N$counts, 0)
lines(mp_N, freq_N, type = "b", pch = 20, col = "orange", lwd = 3)



filepath_Payroll="D:/Payroll Hours - Payroll Hours.csv"
MyData_Payroll <- read.csv(file=filepath_Payroll, header=TRUE, sep=",")
attach(MyData_Payroll)


#Payroll Hours
col_ph_mean_ten_Percent = mean(MyData_Payroll$Hours, trim=1/10)
col_ph_median = median(MyData_Payroll$Hours)
col_ph_Mode = getmode(MyData_Payroll$Hours)
col_ph_IQR = IQR(MyData_Payroll$Hours)
hist(MyData_Payroll$Hours, xlab="Payroll Hours", col="Yellow", border = "Blue")
h_ph <- hist(MyData_Payroll$Hours, xlab="Payroll Hours", col="Yellow", border = "Blue")
mp_ph = c(min(h_ph$mids) - (h_ph$mids[2] - h_ph$mids[1]), h_ph$mids, max(h_ph$mids) + (h_ph$mids[2] - h_ph$mids[1]))
freq_ph = c(0, h_ph$counts, 0)
lines(mp_ph, freq_ph, type = "b", pch = 20, col = "red", lwd = 3)


