setwd("D:/TUM/Semester 1 - WINTER 21/Data Analysis and Visualisation in R/Case Study - The City of Barcelona-20211115/")

library(ggplot2)
library(data.table)
library(magrittr) # Needed for %>% operator
library(tidyr)
library(GGally)
library(pheatmap)
library(mclust)


######################################################################
################    POPULATION     ###################################
######################################################################
population <- fread('data/population.csv')

head(population)

summary(population)

View(population)

## Total Population by Year
population[,sum(Number),by='Year']

## Total Population by Year,District

# ggplot(data = population[,sum(Number),by=c('Year','District.Code')],aes(x=Year,y=V1,label=District.Code)) + 
#   geom_point() + geom_text(aes(label=District.Code),vjust=2)

population[,sum(Number),by=c('Year','District.Code')]

## Not a huge change in population
population[,sum(Number),by=c('Year')]




district_pop_year <- population[,sum(Number),by=c('Year','District.Code')]

district_pop_year[,Year:=factor(Year)]
district_pop_year[,District.Code:=factor(District.Code)]

district_pop_year

ggplot(district_pop_year,aes(y=V1,x= District.Code)) +
  geom_bar(stat="identity") +
  facet_grid(~Year) +
  scale_y_log10()

# x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap")
# # install.packages(x) # warning: uncommenting this may take a number of minutes
# lapply(x, library, character.only = TRUE) # load the required packages

# https://homagetobcn.com/barcelona-immigration-statistics/

# population
# colnames(population)
# 
# 
# subset <- population[,c("District.Code","District.Name","Neighborhood.Code","Neighborhood.Name")]
# 
# head(subset)
# 
# subset[,unique(Neighborhood.Name)]
# 
# subset <- unique(subset,by="Neighborhood.Name")
# 
# write.csv(subset,'District_Neighborhood_Mapping.csv',row.names = FALSE)

######################################################################
################    ACCIDENTS     ###################################
######################################################################


accidents <- fread('data/accidents_2017.csv')

head(accidents)

summary(accidents)

View(accidents)



unique(accidents$`District Name`)
district_accident <- accidents[,.N,by=`District Name`]

colnames(district_accident) = c("District.Name","num")

district_accident[,'District.Name':=factor(District.Name)]
district_accident

ggplot(district_accident,aes(x=District.Name,y=num)) +
  geom_bar(stat='identity')


### Normalise with the population in each district

head(district_accident)

district_pop_2017 <- population[Year==2017,sum(Number),by='District.Name']
setnames(district_pop_2017, 'V1','Pop')
district_pop_2017


district_accident_with_pop <- merge(district_accident,district_pop_2017,by='District.Name')
district_accident_with_pop[,'accidents_per_1000':= (num/Pop)*1000]

district_accident_with_pop
ggplot(district_accident_with_pop,aes(x=District.Name,y=accidents_per_1000)) +
  geom_bar(stat='identity')



######################################################################
################    MAP MAP MAPPITY MAP     ##########################
######################################################################

library(maps)
library(mapdata)
library(rgdal)
library(ggmap)
library(RColorBrewer)

my_spdf <- readOGR(dsn= "geoportal_bcn/ExportToSHP/L%C3%ADmitdistricte.shp",
  verbose=FALSE
)

summary(my_spdf)
length(my_spdf)
head(my_spdf@data)

library(broom)
spdf_fortified <- tidy(my_spdf)

head(spdf_fortified)

# Plot it
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  theme_void()

## Adding labels
cnames <- aggregate(cbind(long, lat) ~ id, data=spdf_fortified, 
                    FUN=function(x)mean(range(x)))
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  geom_text(data=cnames, aes(long, lat, label = id), size=5)+
  theme_void()

## Adding color based on labels
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group,fill=id)) +
  geom_text(data=cnames, aes(long, lat, label = id), size=5)+
  theme_void()

## Trying to get a better color palette
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group,fill=id)) +
  geom_text(data=cnames, aes(long, lat, label = id), size=5)+
  scale_fill_brewer(palette = "Paired")+
  theme_bw()

## District to id map (should be a better way to do this)

district_id_map <- data.table(id=as.character(c(0:9)),
                    District.Name=c(district_accident_with_pop$District.Name[10],
      district_accident_with_pop$District.Name[8],district_accident_with_pop$District.Name[6],
      district_accident_with_pop$District.Name[2],district_accident_with_pop$District.Name[4],
      district_accident_with_pop$District.Name[3],district_accident_with_pop$District.Name[5],
      district_accident_with_pop$District.Name[1],district_accident_with_pop$District.Name[7],
      district_accident_with_pop$District.Name[9]))

district_id_map

test_new_spdf <- merge(as.data.table(spdf_fortified),district_id_map,by="id")
## Adding labels
cnames_test <- aggregate(cbind(long, lat) ~ District.Name, data=test_new_spdf, 
                    FUN=function(x)mean(range(x)))
ggplot() +
  geom_polygon(data = test_new_spdf, aes( x = long, y = lat, group = group,fill=District.Name)) +
  geom_text(data=cnames_test, aes(long, lat, label = District.Name), size=5)+
  scale_fill_brewer(palette = "Paired")+
  theme_bw()


## Adding accidents datat to map
district_accident_with_pop

accident_spdf <- merge(test_new_spdf,district_accident_with_pop,
                       by='District.Name')

accident_spdf
ggplot() +
  geom_polygon(data = accident_spdf, aes( x = long, y = lat, group = group,fill=accidents_per_1000)) +
  geom_text(data=cnames_test, aes(long, lat, label = District.Name), size=5)+
  # scale_fill_gradientn(colors='red')+
  theme_grey()


