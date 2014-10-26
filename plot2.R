## Exploratory Data Analysis
## Course Project 2
## Date: 10/26/2014
## Plot 2

## Question1: Have total emissions from PM2.5 decreased in the Baltimore City,
## Maryland (fips == "24510") from 1999 to 2008? 
## Use the base plotting system to make a plot answering this question.

## Read in the data. Assuming the summarySCC_PM25.rds file is present in
## current working directory

NEI <- readRDS("summarySCC_PM25.rds")

## Filtering the data frame for fips == "24510" ---> Baltimore City only

NEI_Baltimore <- NEI[NEI$fips == "24510",]

## Aggregating the data for Baltimore City

total_PM25_ByYear<- tapply(NEI_Baltimore$Emissions, NEI_Baltimore$year, sum)

## Plotting the graph using base plot

png("plot2.png", width=480, height=480)
plot(names(total_PM25_ByYear), total_PM25_ByYear, type="b",
     xlab="Year", ylab=expression("Total" ~ PM[2.5] ~ "Emissions (tons)"),
     main=expression("Total Baltimore City" ~ PM[2.5] ~ "Emissions by Year"))
dev.off()

## Observations: The X-Y plot clearly shows that for Baltimore City, from 1999 to 2002 there was a 
## downward trend. However, from 2002 to 2005 there was a significant upward trend.However, from 2005 
## to 2008, the Total PM25 emissions have started to come down again

