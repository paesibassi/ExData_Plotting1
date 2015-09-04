# import table as character and numeric values
household_power_consumption <- read.table("household_power_consumption.txt", sep = ";", dec = ".", header = TRUE, colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), na.strings = "?")

#subset 2 specific days: 1/2/2007 & 1/2/2007
household_power_consumption <- subset(household_power_consumption, Date == "1/2/2007" | Date == "2/2/2007")

# convert date and time columns to POSIXlt DateTime columnrm/
household_power_consumption$DateTime <- with(household_power_consumption, as.POSIXlt(strptime(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S")))

#1 histogram with red bars
png(filename = "plot1.png", width = 480, height = 480)
with(household_power_consumption, {
  hist(household_power_consumption$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
})
dev.off()

#2 trend line with weekdays
png(filename = "plot2.png", width = 480, height = 480)
with(household_power_consumption, {
  plot(household_power_consumption$DateTime, household_power_consumption$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
})
dev.off()

#3 multiple lines with colors
png(filename = "plot3.png", width = 480, height = 480)
with(household_power_consumption, {
  plot(DateTime, Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
  lines(DateTime, Sub_metering_2, col = "red")
  lines(DateTime, Sub_metering_3, col = "blue")
  legend(x = "topright", legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), lty = "solid", col = c("black","red","blue"))
})
dev.off()

#4 multiple plots on a 2 x 2 grid
png(filename = "plot4.png", width = 480, height = 480)
par(mfrow = c(2,2))
with(household_power_consumption, {
  plot(DateTime, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power")
  
  plot(DateTime, Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
  
  plot(DateTime, Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
  lines(DateTime, Sub_metering_2, col = "red")
  lines(DateTime, Sub_metering_3, col = "blue")
  legend(x = "topright", legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), lty = "solid", col = c("black","red","blue"), bty = "n")
  
  plot(DateTime, Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power")
})
dev.off()
