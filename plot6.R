## Exploratory Data Analysis
## Course Project 2
## Date: 10/26/2014
## Plot 6

## Question6: Compare emissions from motor vehicle sources in Baltimore City with emissions
## from motor vehicle sources in Los Angeles County, California (fips == "06037").
## Which city has seen greater changes over time in motor vehicle emissions?

## Since we will be using ggplot and ddply, invoking both the libraries

library(ggplot2)
library(plyr)

## Read in the data. Assuming the summarySCC_PM25.rds file is present in
## current working directory

NEI <- readRDS("summarySCC_PM25.rds")
Source_Classification <- readRDS("Source_Classification_Code.rds")

## If we investigate the Source_Classification dataframe, we note that
## the word "Vehicle" can be found under the column "EI.Sector". The following 4
##combinations can be found:
##    1) "Mobile - On-Road Gasoline Light Duty Vehicles"                   
##    2) "Mobile - On-Road Gasoline Heavy Duty Vehicles"
##    3) "Mobile - On-Road Diesel Light Duty Vehicles"                     
##    4) "Mobile - On-Road Diesel Heavy Duty Vehicles            
 

## Since the above lists seems to be pretty comprehensive and covers all kinds of Vehicles
## I am using the above logic. This is my assumption since the Course TA's did not indicate how
## Motor Vehicles can be sourced from SCC Data in the discussion forums

## For doing the above, we will create a vector called Vehicle containing the instances of the
## above 4 Vehicle types

## For this we will use grep function which will search under EI.Sector column of Source_Classification
## dataframe using the search word "Vehicle"

Vehicle <- grep("vehicle",Source_Classification$EI.Sector,value=T,ignore.case=T)

## So now we will filter the Source_Classification dataframe using the Vehicle vector. 
## Please also note that we are using the above filter to get the values of the corresponsing 
## SC_CC variables only.

SC_CC <- subset(Source_Classification, Source_Classification$EI.Sector %in% Vehicle, select= SCC)
  
## Filtering the data frame for fips == "24510" ---> Baltimore City only
## Filtering the data frame for fips == "06037" ---> LA County

NEI_Baltimore <- NEI[NEI$fips == "24510",]
NEI_LA <- NEI[NEI$fips == "06037",]

## Now, will merge (using the merge function) the NEI dataframe with SC_CC dataframe so that NEI Dataframe only contains 
## SCC values contained in SC_CC dataframe. This is done for both Baltimore and LA. This is equivalent to a Join function in SQL.

NEI_Vehicle_Baltimore <- merge(NEI_Baltimore, SC_CC, by.y="SCC")
NEI_Vehicle_LA <- merge(NEI_LA, SC_CC, by.y="SCC")

## Now we will RBind the two data frames
NEI_Combined <- rbind(NEI_Vehicle_Baltimore,NEI_Vehicle_LA)

## Now transform the NEI_Combined dataframe so that we have another column called region
## consisting of Baltimore or LA County

NEI_Transform <- transform(NEI_Combined,
                           region = ifelse(fips == "24510", "Baltimore City", "Los Angeles County"))

## Using ddply to find out the total emissions due to Motor Vehicles for all years for 
## both Baltimore City and LA County
total_Vehicle_Emissions_ByYear <- ddply(NEI_Transform, .(year, region), function(x) sum(x$Emissions))

## Renaming the X Column Name to Emissions

colnames(total_Vehicle_Emissions_ByYear)[3] <- "Emissions"

## In order to plot the changes for both LA and Baltimore over time, we will use 1999 as the base year
## Using 1999 as a base year, we will create normalized values for both LA and Baltimore City

Emissions_1999_Balt <- subset(total_Vehicle_Emissions_ByYear,year == "1999" & region == "Baltimore City")$Emissions
Emissions_1999_LA <- subset(total_Vehicle_Emissions_ByYear,year == "1999" & region == "Los Angeles County")$Emissions

## Normalize all the year Emission values using the 1999 Base Value for Baltimore and LA County.

total_Vehicle_Emissions_ByYear_Norm <- transform(total_Vehicle_Emissions_ByYear, Emissions_Norm = ifelse(region == "Baltimore City",
                                                                                                        Emissions / Emissions_1999_Balt,
                                                                                                        Emissions / Emissions_1999_LA))

## If all options of ggplot2 are not used, it generates some warnings to point to
## user that other options are also available. This is a good feature of ggplot2.
## However since I do not want any warnings to appear, I am suppressing the warnings
## by using Options(warn = -1). The default is Options(warn = 0)

options(warn = -1)

## Plotting the graph using ggplot

png("plot6.png", width=600)
qplot(year, Emissions_Norm, data=total_Vehicle_Emissions_ByYear_Norm, geom="line", color=region) +
  ggtitle(expression("Total" ~ PM[2.5] ~
                       "Motor Vehicle Emissions Normalized to 1999 Levels")) +
  xlab("Year") +
  ylab(expression("Normalized" ~ PM[2.5] ~ "Emissions"))
dev.off()

## Observations: The Normalized plot clearly shows that Total PM 2.5 emissions for Motor Vehicles in Baltimore City
## has decreased significatly from 1999 base level for the period 1999 to 2002 and then at a lower slope between 2002 
## and 2008. However for the same period, the Total PM 2.5 Emissions for Motor Vehicles in LA County has increased 
## significantly starting from 1999 base level to 2005. However, after 2005, the Emissions are coming down and is 
## close to 1999 base level in 2008.

