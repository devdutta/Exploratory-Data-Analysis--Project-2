## Exploratory Data Analysis
## Course Project 2
## Date: 10/26/2014
## Plot 3

## Question3: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
## variable, which of these four sources have seen decreases in emissions from 1999-2008 for 
## Baltimore City? Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting 
## system to make a plot answer this question.

## Since we will be using ggplot and ddply, invoking both the libraries

library(ggplot2)
library(plyr)

## Read in the data. Assuming the summarySCC_PM25.rds file is present in
## current working directory

NEI <- readRDS("summarySCC_PM25.rds")

## Filtering the data frame for fips == "24510" ---> Baltimore City only

NEI_Baltimore <- NEI[NEI$fips == "24510",]

## Aggregating the data for Baltimore City for all the point types (using ddply function)

total_typePM25_ByYear <- ddply(NEI_Baltimore, .(year, type), function(x) sum(x$Emissions))

## Renaming the X Column Name to Emissions

colnames(total_typePM25_ByYear)[3] <- "Emissions"

options(warn = -1)

## Plotting the graph using ggplot

png("plot3.png",width=480,height=480,units="px")
p <- ggplot(total_typePM25_ByYear, aes(x=year, y=Emissions, colour=type)) +
  geom_point(alpha=.3) +
  geom_smooth(alpha=.2, size=1, method="loess") +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
  labs(title=expression("Total" ~ PM[2.5] ~ "Emissions for Baltimore City"))
  

print(p)

dev.off()

## Observations: The plot clearly shows that for Baltimore City, from 1999 to 2008 there was a 
## downward trend for Non-Point, Non-Road and On-Road type sources. However, for Point type source
# from 1999 to 2005 there was a significant upward trend.However, from 2005 
## to 2008, the Total PM25 emissions have started to come down again for Point type source

