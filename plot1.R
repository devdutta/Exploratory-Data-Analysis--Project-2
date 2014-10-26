## Exploratory Data Analysis
## Course Project 2
## Date: 10/26/2014
## Plot 1

## Question1: Have total emissions from PM2.5 decreased in the United States
## from 1999 to 2008? Using the base plotting system, make a plot showing the
## total PM2.5 emission from all sources for each of the years
## 1999, 2002, 2005, and 2008.

## Read in the data. Assuming the summarySCC_PM25.rds file is present in
## current working directory

NEI <- readRDS("summarySCC_PM25.rds")

## Creating the aggregated data for Total PM25 emissions across all years

total_PM25_ByYear <- tapply(NEI$Emissions, NEI$year, sum)

## Plotting the graph using base plot

png("plot1.png", width=480, height=480)
plot(names(total_PM25_ByYear), total_PM25_ByYear, type="b",
     xlab="Year", ylab=expression("Total" ~ PM[2.5] ~ "Emissions (tons)"),
     main=expression("Total US" ~ PM[2.5] ~ "Emissions by Year"))
dev.off()

## Observations: The X-Y plot clearly shows that there is a downward trend 
## in Total PM25 emissions in US from 1999 to 2008