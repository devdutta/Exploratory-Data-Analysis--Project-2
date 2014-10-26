## Exploratory Data Analysis
## Course Project 2
## Date: 10/26/2014
## Plot 5

## Question5: How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

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

NEI_Baltimore <- NEI[NEI$fips == "24510",]

## Now, will merge (using the merge function) the NEI dataframe with SC_CC dataframe so that NEI Dataframe only contains 
## SCC values contained in SC_CC dataframe. This is equivalent to a Join function in SQL.

NEI_Vehicle <- merge(NEI_Baltimore, SC_CC, by.y="SCC")

## Using ddply to find out the total emissions due to Motor Vehicles for all years
total_Vehicle_Emissions_ByYear <- ddply(NEI_Vehicle, .(year), function(x) sum(x$Emissions))

## Renaming the X Column Name to Emissions

colnames(total_Vehicle_Emissions_ByYear)[2] <- "Emissions"

## If all options of ggplot2 are not used, it generates some warnings to point to
## user that other options are also available. This is a good feature of ggplot2.
## However since I do not want any warnings to appear, I am suppressing the warnings
## by using Options(warn = -1). The default is Options(warn = 0)

options(warn = -1)

## Plotting the graph using ggplot

png("plot5.png",width=480,height=480,units="px")
p <- ggplot(total_Vehicle_Emissions_ByYear, aes(x=year,y=Emissions)) +
  geom_point(alpha=.3) +
  geom_smooth(alpha=.2, size=1, method="loess") +
  labs(x="Year", y=expression("Total PM"[2.5]*" Emission (in Tons)")) + 
  labs(title=expression("Total" ~ PM[2.5] ~ "Emissions for Motor Vehicles in Baltimore City"))
  

print(p)

dev.off()

## Observations: The plot clearly shows that Total PM 2.5 emissions for Motor Vehicles in Baltimore City from 1999 to 2002,
## there was a steep downward trend. However, between 2002 and 2005 there was a slight downward trend.
# From 2005 to 2008 however, the downward slope of the trend increased as compared to period 2002 to 2005. 

