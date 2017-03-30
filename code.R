library(ggplot2)
library(animation)
library(dplyr)
library(ggthemes)
library(idbr)
library(devtools)
library(viridis)
library(rgeos)
library(maptools)


#import original crime data and philly cencus tract shapefile
Crime <- read.csv("D:/study/MUSA620/week_9_assignment/PPD_Crime_Incidents_2006-Present.csv")
Pct <- readShapeSpatial("D:/study/MUSA620/week_9_assignment/census-tracts-philly.shp")
plotData1 <- fortify(Pct, region = "GISJOIN")
#filter null crime data
Crime1 <- na.omit(Crime)
#set latitude and longtitude apart by using regular expression
Crime1$lat <- gsub(pattern = "POINT\\s\\((.*)\\s(.*)\\)", replacement = "\\2",x=Crime1$Shape)
Crime1$lon <- gsub(pattern = "POINT\\s\\((.*)\\s(.*)\\)", replacement = "\\1",x=Crime1$Shape)
#colnames(Crime1)

#filter each year crime data for spatial join in arcmap, there should be a easier way but
# I did not figure it out.
Crime1$date <- as.Date(Crime1$Dispatch.Date, format= "%Y-%m-%d")
Crime2009 <- subset(Crime1, Crime1$date >= "2009-01-01" & Crime1$date <= "2009-12-31")
Crime2010 <- subset(Crime1, Crime1$date >= "2010-01-01" & Crime1$date <= "2010-12-31")
Crime2011 <- subset(Crime1, Crime1$date >= "2011-01-01" & Crime1$date <= "2011-12-31")
Crime2012 <- subset(Crime1, Crime1$date >= "2012-01-01" & Crime1$date <= "2012-12-31")
Crime2013 <- subset(Crime1, Crime1$date >= "2013-01-01" & Crime1$date <= "2013-12-31")
Crime2014 <- subset(Crime1, Crime1$date >= "2014-01-01" & Crime1$date <= "2014-12-31")
Crime2015 <- subset(Crime1, Crime1$date >= "2015-01-01" & Crime1$date <= "2015-12-31")
Crime2016 <- subset(Crime1, Crime1$date >= "2016-01-01" & Crime1$date <= "2016-12-31")
#cirme clean data from 2009 - 2016
write.csv(Crime2009, file = "D:/study/MUSA620/week_9_assignment/Crime2009.csv",row.names=FALSE)
write.csv(Crime2010, file = "D:/study/MUSA620/week_9_assignment/Crime2010.csv",row.names=FALSE)
write.csv(Crime2011, file = "D:/study/MUSA620/week_9_assignment/Crime2011.csv",row.names=FALSE)
write.csv(Crime2012, file = "D:/study/MUSA620/week_9_assignment/Crime2012.csv",row.names=FALSE)
write.csv(Crime2013, file = "D:/study/MUSA620/week_9_assignment/Crime2013.csv",row.names=FALSE)
write.csv(Crime2014, file = "D:/study/MUSA620/week_9_assignment/Crime2014.csv",row.names=FALSE)
write.csv(Crime2015, file = "D:/study/MUSA620/week_9_assignment/Crime2015.csv",row.names=FALSE)
write.csv(Crime2016, file = "D:/study/MUSA620/week_9_assignment/Crime2016.csv",row.names=FALSE)

#after spatial join each year's crime data to philly cencus tract in arcmap
#I exported them back to csv and do the left join
#again I think there should be one smarter way  lol

ctp2009 <- read.csv("D:/study/MUSA620/week_9_assignment/N2009.csv")
ctp2010 <- read.csv("D:/study/MUSA620/week_9_assignment/N2010.csv")
ctp2011 <- read.csv("D:/study/MUSA620/week_9_assignment/N2011.csv")
ctp2012 <- read.csv("D:/study/MUSA620/week_9_assignment/N2012.csv")
ctp2013 <- read.csv("D:/study/MUSA620/week_9_assignment/N2013.csv")
ctp2014 <- read.csv("D:/study/MUSA620/week_9_assignment/N2014.csv")
ctp2015 <- read.csv("D:/study/MUSA620/week_9_assignment/N2015.csv")
ctp2016 <- read.csv("D:/study/MUSA620/week_9_assignment/N2016.csv")

new_2009 <- data.frame(FID=ctp2009$FID,GISJOIN=ctp2009$GISJOIN,count=ctp2009$Count_,year=2009)
new_2010 <- data.frame(FID=ctp2010$FID,GISJOIN=ctp2010$GISJOIN,count=ctp2010$Count_,year=2010)
new_2011 <- data.frame(FID=ctp2011$FID,GISJOIN=ctp2011$GISJOIN,count=ctp2011$Count_,year=2011)
new_2012 <- data.frame(FID=ctp2012$FID,GISJOIN=ctp2012$GISJOIN,count=ctp2012$Count_,year=2012)
new_2013 <- data.frame(FID=ctp2013$FID,GISJOIN=ctp2013$GISJOIN,count=ctp2013$Count_,year=2013)
new_2014 <- data.frame(FID=ctp2014$FID,GISJOIN=ctp2014$GISJOIN,count=ctp2014$Count_,year=2014)
new_2015 <- data.frame(FID=ctp2015$FID,GISJOIN=ctp2015$GISJOIN,count=ctp2015$Count_,year=2015)
new_2016 <- data.frame(FID=ctp2016$FID,GISJOIN=ctp2016$GISJOIN,count=ctp2016$Count_,year=2016)

#merge crime data of every year in one dataset for GIF preparetion
merged_data <- rbind(new_2009,new_2010,new_2011,new_2012,new_2013,new_2014,new_2015,new_2016)
plotData <- left_join(plotData1, merged_data, by=c("id" = "GISJOIN"))
summary(plotData$count)

saveGIF({
  for (i in 2009:2016) {
    year <- as.character(i)
    year_data <- subset(plotData, year == i)
    
    yearplot <- ggplot(data = year_data  , aes(x = long, y = lat, group = group,
                                                 fill = count) ) +
      geom_polygon(color = "grey10", size = 0.01,alpha = 0.8) +
      # set the projection (Mercator in this case)
      coord_map() +
      #set the color scale
      scale_fill_continuous(limits=c(14, 2000), breaks=seq(0, 2000,by=500),
                            high="#ff0000")+
      #scale_fill_brewer(palette="Greens") +
      labs(title = paste0("Philadelphia, ",year),
           fill = "Crime Number") +
      #set the plot theme
      theme_void() +
      #theme_bw() +
      theme(text = element_text(size = 8,color = "white"),
            plot.title = element_text(size = 12, face = "bold",color = "black"),
            panel.background = element_rect(fill = "black"),
            plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "in"),
            legend.text = element_text(size = 7,color = "white"),
            legend.position = c(0.8, 0.25))
    
    print(yearplot)
  }
}, movie.name = 'trial.gif', interval = 0.3, ani.width = 700, ani.height = 600)


#I also tried to export each year's map seperately and make gif online, just a try and it works as well
map1 <- ggplot() +
  geom_polygon(data = plotData1, aes(x = long, y = lat, group = group,
                                    fill = Count_), color = "black", size = 0.1) +
  coord_map() # Mercator projection
map1
ggsave(file="D:/study/MUSA620/week_9_assignment/00_output/09.png")


ctp2010 <- read.csv("D:/study/MUSA620/week_9_assignment/N2010.csv")
plotData2 <- left_join(data1, ctp2010, by=c("id" = "GISJOIN"))
map2 <- ggplot() +
  geom_polygon(data = plotData2, aes(x = long, y = lat, group = group,
                                     fill = Count_), color = "black", size = 0.1) +
  coord_map() # Mercator projection
map2
ggsave(file="D:/study/MUSA620/week_9_assignment/00_output/10.png")











