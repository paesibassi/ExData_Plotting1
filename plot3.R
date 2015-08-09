# import table as character and numeric values
household_power_consumption <- read.table("household_power_consumption.txt", sep = ";", dec = ".", header = TRUE, colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), na.strings = "?")

#subset 2 specific days: 1/2/2007 & 1/2/2007
household_power_consumption <- subset(household_power_consumption, Date == "1/2/2007" | Date == "2/2/2007")

# convert date and time columns to POSIXlt DateTime columnrm/
household_power_consumption$DateTime <- with(household_power_consumption, as.POSIXlt(strptime(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S")))

#3 multiple lines with colors
png(filename = "plot3.png", width = 480, height = 480)
with(household_power_consumption, {
  plot(DateTime, Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
  lines(DateTime, Sub_metering_2, col = "red")
  lines(DateTime, Sub_metering_3, col = "blue")
  legend(x = "topright", legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), lty = "solid", col = c("black","red","blue"))
})
dev.off()
