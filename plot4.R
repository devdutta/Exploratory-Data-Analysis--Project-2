## Exploratory Data Analysis
## Course Project 2
## Date: 10/26/2014
## Plot 4

## Question4: Across the United States, how have emissions from coal
## combustion-related sources changed from 1999-2008?

## Since we will be using ggplot and ddply, invoking both the libraries

library(ggplot2)
library(plyr)

## Read in the data. Assuming the summarySCC_PM25.rds file is present in
## current working directory

NEI <- readRDS("summarySCC_PM25.rds")
Source_Classification <- readRDS("Source_Classification_Code.rds")

## If we investigate the Source_Classification dataframe, we note that
## the word "Coal" can be found under the column "EI.Sector". The following three
##combinations can be found:
##    1) "Fuel Comb - Electric Generation - Coal" 
##    2) "Fuel Comb - Industrial Boilers, ICEs - Coal"
##    3) "Fuel Comb - Comm/Institutional - Coal" 

## Now we will create a vector called Coal_Combust containing the instances of the
## above 3 values only

## For this we will use grep function which will search under EI.Sector column of Source_Classification
## dataframe using the search word "Coal"

Coal_Combust <- grep("coal",Source_Classification$EI.Sector,value=T,ignore.case=T)

## So now we will filter the Source_Classification dataframe using the Coal_Combust vector. 
## Please also note that we are using the above filter to get the values of the corresponsing 
## SCC variables only.

SC_CC <- subset(Source_Classification, Source_Classification$EI.Sector %in% Coal_Combust, select= SCC)
  

## Now, will merge (using the merge function) the NEI dataframe with SC_CC dataframe so that NEI Dataframe only contains 
## SCC values contained in SC_CC dataframe

NEI_CC <- merge(NEI, SC_CC, by.y="SCC")

## Using ddply to find out the total emissions dure to coal combustion for all years
total_CC_Emissions_ByYear <- ddply(NEI_CC, .(year), function(x) sum(x$Emissions))

## Renaming the X Column Name to Emissions

colnames(total_CC_Emissions_ByYear)[2] <- "Emissions"

## If all options of ggplot2 are not used, it generates some warnings to point to
## user that other options are also available. This is a good feature of ggplot2.
## However since I do not want any warnings to appear, I am suppressing the warnings
## by using Options(warn = -1). The default is Options(warn = 0)

options(warn = -1)

## Plotting the graph using ggplot

png("plot4.png",width=480,height=480,units="px")
p <- ggplot(total_CC_Emissions_ByYear, aes(x=year,y=Emissions/10^6)) +
  geom_point(alpha=.3) +
  geom_smooth(alpha=.2, size=1, method="loess") +
  labs(x="Year", y=expression("Total PM"[2.5]*" Emission (in Million Tons)")) + 
  labs(title=expression("Total" ~ PM[2.5] ~ "Emissions for Coal Combustion"))
  

print(p)

dev.off()

## Observations: The plot clearly shows that Total PM 2.5 emissions for Coal Combustion from 1999 to 2002,
## there was a slight downward trend. However, between 2002 and 2005 there was a slight upward trend.
# From 2005 to 2008 howeve, there was a significant downward trend in Emission Numbers. Please also note that 
## in the plot above, the unit used for Emission numbers is Million of Tons (10^6 tons)

