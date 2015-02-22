# Install required packages
install.packages('ggmap')
install.packages('ggplot2')

# Load packages
library(ggmap)
library(ggplot2)

getwd()
setwd('/Users/Ankoor/Documents/Git/Map-Plotting/LosAngelesCrime')

# Read Data
crime <- read.csv("LAPD_Crime_and_Collision_Raw_Data_for_2013.csv", stringsAsFactors = FALSE)

## Latitude and Longitude data is in string format: "(34.0496, -118.265)". Need to clean this column by removing "(" and ")" then splitting the string "34.0496, -118.265" into separate Longitude and Latitude columns based on comma and single space: ", "
# Cleaning Location Column to extract Longitude and Latitude
crime$Location <- gsub("\\(|\\)", "", crime$Location.1)
temp_1 <- sapply(strsplit(crime$Location, ", ", fixed = TRUE), "[",1)
temp_2 <- sapply(strsplit(crime$Location, ", ", fixed = TRUE), "[",2)
crime$Lat <- as.numeric(temp_1)
crime$Long <- as.numeric(temp_2)

# Keeping necessary Columns
names(crime)
keep <- c('DATE.OCC', 'TIME.OCC', 'AREA.NAME', 'Crm.Cd','Crm.Cd.Desc', 'Lat', 'Long')
crime <- crime[keep]

## Date is in string format: "12/31/2013". Need to convert date from "string" to "date" format used in R and then get weekdays.
# Cleaning Date Column
crime$DATE.OCC <- as.Date(crime$DATE.OCC, "%m/%d/%Y")
crime$Day <- weekdays(crime$DATE.OCC)

## Time data is in 24-hour format (Military time). To visualize temporal variation in crimes I decided to assign Time in 4 quarters of a day as follows: 0 to 600 hours = First Quarter, 601 to 1200 hours = Second Quarter, 1201 to 1800 hours = Third Quarter and 1801 to 2400 hours = Fourth Quarter
# Quarters in a day
crime$Quarter <- crime$TIME.OCC
crime$Quarter[which(crime$TIME.OCC < 600)] <- 'First'
crime$Quarter[which(crime$TIME.OCC >= 600 & crime$TIME.OCC < 1200)] <- 'Second'
crime$Quarter[which(crime$TIME.OCC >= 1200 & crime$TIME.OCC < 1800)] <- 'Third'
crime$Quarter[which(crime$TIME.OCC >= 1800)] <- 'Fourth'

## Now creating maps
# Get Longitude and Latitude 
geocode("Los Angeles") 

# Get Google Map
LA = c(lon = -118.2437, lat =  34.05223)
LA.map = get_map(location = LA, zoom = 11, maptype = 'terrain')

# Plotting Crime Density Map
pdf("LA_Crime_Density.pdf", width = 11, height = 11)
ggmap(LA.map, extent = "normal", maprange=FALSE) %+% crime + aes(x = Long, y = Lat) + 
        stat_density2d(aes(fill = ..level.., alpha = ..level..), size = 5, bins = 20, geom = 'polygon') + 
        scale_fill_continuous(low = 'black', high = 'red', name = "Crime\nDensity") +
        scale_alpha(range = c(0.05, 0.25), guide = FALSE) + 
        coord_map(projection = "mercator", 
                  xlim = c(attr(LA.map, "bb")$ll.lon, attr(LA.map, "bb")$ur.lon), 
                  ylim = c(attr(LA.map, "bb")$ll.lat, attr(LA.map, "bb")$ur.lat)) + 
        theme(legend.justification=c(1,0), legend.position=c(1,0),axis.title = element_blank(), text = element_text(size = 14)) 
dev.off()


##Vehicle Collision/Accident Maps
# Creating subset of Crime data based on Crime Code Description for Collision
collision <- subset(crime, Crm.Cd.Desc == 'TRAFFIC DR #')
names(collision)[5]<-"Collision"

# Get Stamen Map
LA.map = qmap(location = LA, zoom = 11, source = "stamen", maptype = 'toner')

# Plotting Collision Map (I used color = #cb181d" from Color Brewer)
pdf("LA_Collision.pdf", width = 11, height = 11)
LA.map + geom_point(data = collision, aes(x = Long, y = Lat), size = 2, alpha = 0.1, color = "#cb181d")
dev.off()

# Plotting Collision Map to Visualize Weekday Variation in Collisions
pdf("LA_Collision_Weekday_Variation.pdf", width = 11, height = 11)
LA.map + geom_point(data = collision, aes(x = Long, y = Lat), size = 2, alpha = 0.1, color = "#0c2c84") + facet_wrap(~ Day)
dev.off()

# Plotting Collision Map to Visualize Temporal (Quarter based) Variation in Collisions
pdf("LA_Collision_Temporal_Variation.pdf", width = 11, height = 11)
LA.map + geom_point(data = collision, aes(x = Long, y = Lat), size = 2, alpha = 0.1, color = "#0c2c84") + facet_wrap(~ Quarter)
dev.off()

# Plotting Collision Density Map
geocode("Hollywood") 
LA = c(lon = -118.3287, lat =  34.09281)
LA.map = get_map(location = LA, zoom = 11, maptype = 'terrain')

pdf("LA_Collision_Density.pdf", width = 11, height = 11)
ggmap(LA.map, extent = "normal", maprange=FALSE) %+% collision + aes(x = Long, y = Lat) + 
        stat_density2d(aes(fill = ..level.., alpha = ..level..), size = 2, bins = 15, geom = 'polygon') + 
        scale_fill_gradient(low = "red", high = "#081d58", name = "Collision\nDensity") + 
        scale_alpha(range = c(0.05, 0.3), guide = FALSE) + 
        coord_map(projection = "mercator", 
                  xlim = c(attr(LA.map, "bb")$ll.lon, attr(LA.map, "bb")$ur.lon), 
                  ylim = c(attr(LA.map, "bb")$ll.lat, attr(LA.map, "bb")$ur.lat)) + 
        theme(legend.justification=c(1,0), legend.position=c(1,0),axis.title = element_blank(), text = element_text(size = 14))
dev.off()

# Creating subset of Crime data based on Crime Code Description for Violent Crimes
violent <- subset(crime, Crm.Cd.Desc == 'ROBBERY' | Crm.Cd.Desc == 'ASSAULT WITH DEADLY WEAPON, AGGRAVATED ASSAULT' |
                          Crm.Cd.Desc == 'RAPE, ATTEMPTED' | Crm.Cd.Desc == 'CRIMINAL HOMICIDE' | 
                          Crm.Cd.Desc == 'CRIMINAL HOMICIDE' | Crm.Cd.Desc == 'ASSAULT WITH DEADLY WEAPON ON POLICE OFFICER' |
                          Crm.Cd.Desc == 'RAPE, FORCIBLE' | Crm.Cd.Desc == 'HOMICIDE (NON-UCR)')

names(violent)[5] <-"Violent"

violent$Violent <- factor(violent$Violent)

# Plotting Violent Crime Density Map
geocode("Vernon, CA")
LA = c(lon = -118.2301, lat =  34.0039)
LA.map = get_map(location = LA, zoom = 12, maptype = 'terrain')

pdf("LA_Violent_Crime_Density.pdf", width = 11, height = 11)
ggmap(LA.map, extent = "normal", maprange=FALSE) %+% violent + aes(x = Long, y = Lat) + 
        stat_density2d(aes(fill = ..level.., alpha = ..level..), size = 2, bins = 10, geom = 'polygon') + 
        scale_fill_gradient(low = "black", high = "red", name = "Violent Crime\nDensity") + 
        scale_alpha(range = c(0.05, 0.3), guide = FALSE) + 
        coord_map(projection = "mercator", 
                  xlim = c(attr(LA.map, "bb")$ll.lon, attr(LA.map, "bb")$ur.lon), 
                  ylim = c(attr(LA.map, "bb")$ll.lat, attr(LA.map, "bb")$ur.lat)) + 
        theme(legend.justification=c(1,0), legend.position=c(1,0),axis.title = element_blank(), text = element_text(size = 14)) 
dev.off()

# Plotting Map to Visualize Weekday Variation in Violent Crimes
LA.map <- qmap(location = LA, zoom = 11, source = "stamen", maptype = 'toner')

pdf("LA_Violent_Crime_Weekday_Variation.pdf", width = 11, height = 11)
LA.map + geom_point(data = violent, aes(x = Long, y = Lat), size = 2, alpha = 0.1, color = "red") + facet_wrap(~ Day)
dev.off()






