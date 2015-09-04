# Load libraries
library("dplyr")
library("ggplot2")

# Download and extract data file from Internet
download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
              destfile = "./Data_for_Peer_Assessment.zip", method = "curl")
unzip(zipfile = "./Data_for_Peer_Assessment.zip")

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? ----
PM2.5_year <- NEI %>%
  group_by(year) %>%
  summarize(total_emissions = sum(Emissions)) # sums up total emissions
png(filename = "plot1.png", width = 480, height = 480)
with(PM2.5_year, {
  barplot <- barplot(total_emissions) # stores barplot in an object to get the x values
  lm <- lm(total_emissions~year)
  # builds a bar plot to show emissions per year and add a linear regression to show negative slope
  barplot(total_emissions, names.arg = year, main = "PM2.5 total emissions", ylab = "Total emissions")
  lines(x = barplot, y = lm$fitted.values, lty = "dotted", lwd = 2, col = "dodgerblue")
  points(x = barplot, y = lm$fitted.values, col = "dodgerblue")
})
dev.off()
     
## 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? ----
PM2.5_Baltimore_year <- NEI %>%
  filter(fips == "24510") %>% # subsets for Baltimore City county code
  group_by(year) %>%
  summarize(total_emissions = sum(Emissions)) # sums up total emissions
png(filename = "plot2.png", width = 480, height = 480)
with(PM2.5_Baltimore_year, {
  barplot <- barplot(total_emissions) # stores barplot in an object to get the x values
  lm <- lm(total_emissions~year)
    # builds a bar plot to show emissions per year and add a linear regression to show slope
  barplot(total_emissions, names.arg = year, main = "PM2.5 emissions in Baltimore", ylab = "Total emissions")
  lines(x = barplot, y = lm$fitted.values, lty = "dotted", lwd = 2, col = "dodgerblue")
  points(x = barplot, y = lm$fitted.values, col = "dodgerblue")
})
dev.off()

## Of the four types of sources indicated by the type variable,
## 3. which of these four sources have seen decreases/increases in emissions from 1999–2008 for Baltimore City? ----
PM2.5_Baltimore_TY <- NEI %>%
  filter(fips == "24510") %>% # subsets for Baltimore City county code
  group_by(type, year) %>%
  summarize(total_emissions = sum(Emissions)) # sums up total emissions, per type
png(filename = "plot3.png", width = 480, height = 480)
# builds a plot with a trendline for each type, and adds a linear regression line to show slope of each
ggplot(PM2.5_Baltimore_TY, aes(year, total_emissions)) +
  geom_line(aes(colour = PM2.5_Baltimore_TY$type), size = 1) +
  geom_smooth(method = 'lm', aes(colour = PM2.5_Baltimore_TY$type), fill = NA, size = 0.75, linetype = "dotted") +
  labs(title = "PM2.5 emissions in Baltimore", x = "Year", y = "Total emissions", colour = "Type of source") +
  scale_x_continuous(breaks = seq(1999, 2008, 3)) +
  ylim(0, max(PM2.5_Baltimore_TY$total_emissions))
dev.off()

## 4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008? ----
# after some exploration in the SCC data frame, it looked like the best option to subset was by using EI.Sector
# finds SCC codes for Coal combustion by looking at EI.Sector field
SCC_Coal <- filter(SCC, grepl("coal", EI.Sector, ignore.case = TRUE)) 
PM2.5_Coal <- NEI %>%
  filter(SCC %in% SCC_Coal$SCC) %>% # subsets only Coal rows
  group_by(year) %>%
  summarize(total_emissions = sum(Emissions)) # sums totals emissions by year
png(filename = "plot4.png", width = 480, height = 480)
# builds a barplot with emissions per year, and adds a linear regression line to show negative slope
ggplot(PM2.5_Coal, aes(year, total_emissions)) +
  geom_bar(stat="identity") +
  geom_smooth(method = 'lm', fill = NA, size = 1, linetype = "dotted") +
  labs(title = "PM2.5 emissions due to Coal combustion", x = "Year", y = "Total emissions") +
  scale_x_continuous(breaks = seq(1999, 2008, 3)) +
  ylim(0, max(PM2.5_Coal$total_emissions)*1.1)
dev.off()

## 5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City? ----
# after some exploration in the SCC data frame, it looked like the best option to subset was by using EI.Sector
PM2.5_Motor <- filter(NEI, fips == "24510") # subsets for Baltimore
PM2.5_Motor <- merge(PM2.5_Motor, SCC[,c("SCC","EI.Sector")], by = "SCC") %>% # inner join subsetted NEI and SCC
  filter(grepl("vehicle", EI.Sector, ignore.case = TRUE)) %>% # finds motor vehicles by looking at EI.Sector field
  group_by(year) %>%
  summarize(total_emissions = sum(Emissions)) # sums totals emissions by year
png(filename = "plot5.png", width = 480, height = 480)
# builds a barplot with emissions per year, and adds a linear regression line to show negative slope
ggplot(PM2.5_Motor, aes(year, total_emissions)) +
  geom_bar(stat="identity") +
  geom_smooth(method = 'lm', fill = NA, size = 1, linetype = "dotted") +
  labs(title = "PM2.5 emissions due to Vehicles in Baltimore", x = "Year", y = "Total emissions") +
  scale_x_continuous(breaks = seq(1999, 2008, 3)) +
  ylim(0, max(PM2.5_Motor$total_emissions)*1.1)
dev.off()

## Compare emissions from motor vehicle sources in Baltimore City
## with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037").
## 6. Which city has seen greater changes over time in motor vehicle emissions? ----
# after some exploration in the SCC data frame, it looked like the best option to subset was by using EI.Sector
PM2.5_MotorC <- filter(NEI, fips == "24510" | fips == "06037") # subsets for Baltimore and Los Angeles
PM2.5_MotorC <- merge(PM2.5_MotorC, SCC[,c("SCC","EI.Sector")], by = "SCC") %>% # inner join subsetted NEI and SCC
  filter(grepl("vehicle", EI.Sector, ignore.case = TRUE)) %>% # finds motor vehicles by looking at EI.Sector field
  group_by(fips, year) %>%
  summarize(total_emissions = sum(Emissions)) # sums totals emissions by year
png(filename = "plot6.png", width = 480, height = 480)
# builds a trendline with emissions per year in the two Counties,
# adds a linear regression line to show slope for each, and a ribbon to show maximum variance
# commented below, another possible solution
ggplot(PM2.5_MotorC, aes(year, total_emissions)) +
  geom_line(aes(colour = PM2.5_MotorC$fips), size = 1) +
  geom_smooth(method = 'lm', aes(colour = PM2.5_MotorC$fips), fill = NA, size = 0.75, linetype = "dotted") +
  geom_ribbon(aes(ymin = min(PM2.5_MotorC$total_emissions[fips == "06037"]),
                  ymax = max(PM2.5_MotorC$total_emissions[fips == "06037"])), alpha = 0.2) +
  geom_ribbon(aes(ymin = min(PM2.5_MotorC$total_emissions[fips == "24510"]),
                  ymax = max(PM2.5_MotorC$total_emissions[fips == "24510"])), alpha = 0.2) +
  labs(title = "PM2.5 emissions due to Vehicles", x = "Year", y = "Total emissions", colour = "County") +
  scale_x_continuous(breaks = seq(1999, 2008, 3)) +
  ylim(0, max(PM2.5_MotorC$total_emissions))
dev.off()

## Another possible solution to show normalized values instead of absolutes.
## The best solution depends on the question (vague in the provided formulation).
# Normalize values per County
PM2.5_MotorC <- rbind(
  PM2.5_MotorC %>%
    filter(fips == "06037") %>%
    mutate(norm_emissions = (total_emissions - mean(total_emissions)) / sd(total_emissions)),
  PM2.5_MotorC %>%
    filter(fips == "24510") %>%
    mutate(norm_emissions = (total_emissions - mean(total_emissions)) / sd(total_emissions))
)
ggplot(PM2.5_MotorC, aes(year, norm_emissions)) +
  geom_line(aes(colour = PM2.5_MotorC$fips), size = 1) +
  geom_smooth(method = 'lm', aes(colour = PM2.5_MotorC$fips), fill = NA, size = 0.75, linetype = "dotted") +
  labs(title = "PM2.5 emissions due to Vehicles", x = "Year", y = "Normalized total emissions", colour = "County") +
  scale_x_continuous(breaks = seq(1999, 2008, 3))