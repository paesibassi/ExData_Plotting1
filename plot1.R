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
