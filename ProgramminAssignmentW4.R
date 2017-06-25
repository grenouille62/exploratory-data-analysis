library(sqldf)
library(ggplot2)
# Load datas into dataframes
path = file.path(getwd(), "exdata_data_NEI_data")
NEI <- readRDS(file.path(path,"summarySCC_PM25.rds"))
SCC <- readRDS(file.path(path,"Source_Classification_Code.rds"))

# Q1 : Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# compute total emission by year using agregate sql function into a dataframe. The sum is 
USsumByYear <- sqldf( "select year, sum(Emissions) emissions from NEI group by year")
# plot the evolution graph from the dataframe USsumByYear above
png(width = 480, height = 480, filename = file.path(path,"Plot1-US-Emissions-evolution.png"))
plot(x=sumByYear$year, sapply(sumByYear$emissions,function(x) {x/1000}), type="o", xlim=c(min(sumByYear$year),max(sumByYear$year)), xlab = "Year", ylab = "Emissions (in kilotons)", main = paste("United States - PM2.5 emission evolution between", min(sumByYear$year), "and", max(sumByYear$year)), xaxt="n")
axis(at = c(sumByYear$year), side=1)
dev.off()

# Q2 : Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == 24510) from 1999 to 2008?
# compute total emission by year for rows having fips = 24510, using agregate sql function into a dataframe
baltimoreSumByYear <- sqldf( "select year, sum(Emissions) emissions from NEI where fips = '24510' group by year")
# plot the evolution graph from the dataframe baltimoreSumByYear above
png(width = 480, height = 480, filename = file.path(path,"Plot2-Baltimore-Emissions-evolution.png"))
plot(x=baltimoreSumByYear$year, baltimoreSumByYear$emissions, type="o", xlim=c(min(baltimoreSumByYear$year),max(baltimoreSumByYear$year)), xlab = "Year", ylab = "Emissions (in tons)", main = "Baltimore City - PM2.5 emission evolution" , xaxt="n")
axis(at = c(baltimoreSumByYear$year), side=1)
dev.off()

# Q3a: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
# compute total emission by year and type for rows having fips = 24510, using agregate sql function into a dataframe
baltimoreSumByYearType <- sqldf( "select year, type, sum(Emissions) emissions from NEI where fips = '24510' group by year, type")
# plot the evolution graphs from the dataframe baltimoreSumByYearType above with ggplot
# each graph shows the emission evolution of the related type with regression line
# Add plot title, labels of x, y and lengend
# makeup plot title
png(width = 960, height = 960, filename = file.path(path,"Plot3-Baltimore-Emissions-evolution-by-Type.png"))
evolutionGraphs <- ggplot(data=baltimoreSumByYearType, aes(x=year, y=emissions, colour=type)) + geom_line(size=1) + geom_point() + 
  labs(title="Baltimore PM2.5 emissions evolution by Type", y = "Emissions (in tons)", x="Year", color="Type") + facet_grid(. ~ type) +
  geom_smooth(method = "lm", se=FALSE, size=0.5) +
  theme(plot.title = element_text(family = "Arial", color="#000000", face="bold", size=16, hjust=0.5))
print(evolutionGraphs)
dev.off()
# Answer = NONPOINT, NON-ROAD, ON-ROAD decrease  

# Q3b: Which have seen increases in emissions from 1999–2008?
# Answer : the POINT source in 2008 increases compared to 1999. From 1999 to 2005, the emission of this source have seen increases; 
# from 2005 to 2008, it have seen decreases but it's stay higher than 1999

# Q4 : Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
# 1/ find all Coal combustion realated SCC, basing on "Short name" variable
sccCoalRelated <- SCC[grep(SCC[,"Short.Name"],pattern = ".*((comb){1,}.*(coal){1,}.*).*", ignore.case = TRUE, perl = TRUE),"SCC"]
# 2/ extract all NEI rows which have SCC in the list above
neiCoalRelated <- NEI[NEI[,"SCC"] %in% sccCoalRelated,]
# 3/ compute total emission by year using agregate sql function
USCoalSumByYear <- sqldf( "select year, sum(Emissions) emissions from neiCoalRelated group by year")
# plot the evolution graph of coal emission
png(width = 480, height = 480, filename = file.path(path,"Plot4-US-Coal-Emissions-evolution.png"))
plot(x=USCoalSumByYear$year, sapply(USCoalSumByYear$emissions,function(x) {x/1000}), type="o", xlim=c(min(USCoalSumByYear$year),max(USCoalSumByYear$year)), 
     xlab = "Year", ylab = "Coal emissions (in kilotons)", main = "United States - PM2.5 Coal emission evolution", xaxt="n")
axis(at = c(USCoalSumByYear$year), side=1)
dev.off()

# Q5 : How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
# 1/ find all motor vehicle realated SCC, basing on "Short name" variable
sccMotorVehicleRelated <- SCC[grep(SCC[,"SCC.Level.Two"],pattern = "vehicle", ignore.case = TRUE, perl = TRUE),"SCC"]
# 2/ extract all NEI rows which have SCC in the list above
neiMotorVehicleRelated <- NEI[NEI[,"SCC"] %in% sccMotorVehicleRelated,]
# 3/ compute total emission by year using agregate sql function
BaltimoreMotorVehicleSumByYear <- sqldf( "select year, sum(Emissions) emissions from neiMotorVehicleRelated where fips = '24510'  group by year")
# plot the evolution graph of motor vehicle emission
png(width = 480, height = 480, filename = file.path(path,"Plot5-Baltimore-Motor vehicle-Emissions-evolution.png"))
plot(x=BaltimoreMotorVehicleSumByYear$year, y=BaltimoreMotorVehicleSumByYear$emissions, type="o", xlim=c(min(BaltimoreMotorVehicleSumByYear$year),max(BaltimoreMotorVehicleSumByYear$year)), 
     xlab = "Year", ylab = "Motor vehicle emissions (in tons)", main = "Baltimore - PM2.5 Motor Vehicle emission evolution", xaxt="n")
axis(at = c(BaltimoreMotorVehicleSumByYear$year), side=1)
dev.off()

# Q6a : Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == 06037)
#BaltimoreLAMotorVehicleSumByYear <- sqldf( "select year, fips, sum(Emissions) emissions from neiMotorVehicleRelated where (fips = '24510' or fips ='06037')  group by year, fips")
#BaltimoreLAMotorVehicleSumByYear$fips <- sapply(BaltimoreLAMotorVehicleSumByYear$fips, function(x) {if (x=="06037") "Los Angeles" else "Baltimore"})
#comparisonBALAGraphs <- ggplot(data=BaltimoreLAMotorVehicleSumByYear, aes(x=year, y=emissions, colour=fips)) + geom_line(size=1) + geom_point() + 
#  labs(title="Baltimore/LA PM2.5 Vehicles emissions evolution comparison", y = "Emissions (in tons)", x="Year", color="City") +
#  theme(plot.title = element_text(family = "Arial", color="#000000", face="bold", size=16, hjust=0.5)) 
#print(comparisonBALAGraphs)
# Q6b : Which city has seen greater changes over time in motor vehicle emissions?
# plot Baltimore and Los Angeles emission side by side with regression line to observ changes  
install.packages("Rmisc")
library(Rmisc)
png(width = 480*2, height = 480, filename = file.path(path,"Plot6-Baltimore-LA-Motor vehicle-Emissions-change-comparison.png"))
BaltimoreMotorVehicleSumByYearGraph <- ggplot(data=BaltimoreLAMotorVehicleSumByYear[BaltimoreLAMotorVehicleSumByYear$fips=="Baltimore",], aes(x=year, y=emissions)) + 
  geom_line(size=1, col="red") + geom_smooth(method = "lm", size=0.5, se=FALSE) + 
  labs(title="Baltimore Motor vehicle emissions evolution", y="Emissions (in tons)", x="Year") +
  theme(plot.title = element_text(family = "Arial", color="#000000", face="bold", size=12, hjust=0.5))
LAMotorVehicleSumByYearGraph <- ggplot(data=BaltimoreLAMotorVehicleSumByYear[BaltimoreLAMotorVehicleSumByYear$fips=="Los Angeles",], aes(x=year, y=emissions)) + 
  geom_line(size=1, col="green") + geom_smooth(method = "lm", size=0.5, se=FALSE) + 
  labs(title="LA Motor vehicle emissions evolution", y="Emissions (in tons)", x="Year") +
  theme(plot.title = element_text(family = "Arial", color="#000000", face="bold", size=12, hjust=0.5))
multiplot(BaltimoreMotorVehicleSumByYearGraph, LAMotorVehicleSumByYearGraph, cols = 2)
dev.off()
