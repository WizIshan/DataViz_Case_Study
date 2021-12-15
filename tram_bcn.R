#Case Study
library(RColorBrewer)
library(ggplot2)
library(data.table)
library(magrittr) # Needed for %>% operator library(tidyr)
library(ggrepel)
library(GGally)
library(tidyr)
library(pheatmap)
library(factoextra)
library(dplyr)

setwd("/Users/franciscalagos/Library/Mobile Documents/com~apple~CloudDocs/Documents/TUM/1st Semester/DataViz/Case Study")
transports <- fread("data/transports.csv")
transports <- rename_with(transports, tolower)
transports <- rename(transports, "district_name" = "district.name")
transports <- rename(transports, "neighborhood_name" = "neighborhood.name")
#colnames(transports)

#Tram via
tram <- transports[transport == "Tram"]
tram$code <- NULL
tram$transport <- NULL
tram <- separate(tram, col=station, into=c("line", "station_name"), remove = TRUE , sep =' - ')
tram <- separate(tram, col=line, into=c("line", "line_number"), remove = TRUE , sep =' ')
tram <- separate(tram, col=station_name, into = c("station_name","nothing"), remove = TRUE, sep ="-")
tram$line <- NULL
tram$nothing <- NULL
#unique(tram$line_number)
tram[tram$line_number=="(T1,T2,T3)", "line_number"] <- "T1,T2,T3"
tram[tram$line_number=="(T1,T2)", "line_number"] <- "T1,T2"
tram[tram$line_number=="(T2)", "line_number"] <- "T2"
tram[tram$line_number=="(T3)", "line_number"] <- "T3"
tram[tram$line_number=="(T4)", "line_number"] <- "T4"
tram[tram$line_number=="(T4,", "line_number"] <- "T4"
tram[tram$line_number=="(T5)", "line_number"] <- "T5"
tram[tram$line_number=="(T5,", "line_number"] <- "T5"
tram[tram$line_number=="(T6)", "line_number"] <- "T6"

tram_completed <- fread("complementary_data/bcn_tram.csv")
tram_left_merge <- merge(tram_completed, tram, by = "station_name", all.x = TRUE) 
tram_left_merge$line_number <- NULL

tram_left_merge[tram_left_merge$station_name=="EL MARESME", "neighborhood_name"] <- "Diagonal Mar i el Front Marítim del Poblenou"
tram_left_merge[tram_left_merge$station_name=="EL MARESME", "district_name"] <- "Sant Martí"

tram_count_1 <- tram_left_merge[,.N,by='district_name']
tram_count_1<- tram_count_1[!(tram_count$N==26),] 
tram_count_1<- tram_count_1[!(tram_count$N==5),] 

new_row_1 <- data.frame("Gràcia", 0)
names(new_row_1) <- c("district_name", "N")
new_row_2 <- data.frame("Horta-Guinardó", 0)
names(new_row_2) <- c("district_name", "N")
new_row_3 <- data.frame("Sarrià-Sant Gervasi", 0)
names(new_row_3) <- c("district_name", "N")
new_row_4 <- data.frame("Sants-Montjuïc", 0)
names(new_row_4) <- c("district_name", "N")
new_row_5 <- data.frame("Ciutat Vella", 0)
names(new_row_5) <- c("district_name", "N")
new_row_6 <- data.frame("Sant Andreu", 0)
names(new_row_6) <- c("district_name", "N")
new_row_7 <- data.frame("Nou Barris", 0)
names(new_row_7) <- c("district_name", "N")

tram_count <- rbind(tram_count_1, new_row_1, new_row_2, new_row_3, new_row_4, new_row_5, new_row_6, new_row_7)
tram_count<- tram_count[!(tram_count$N==26),] 
tram_count<- tram_count[!(tram_count$N==5),] 
######################################################################
########################    MAP      #################################
######################################################################

library(maps)
library(mapdata)
library(rgdal)
library(ggmap)
library(RColorBrewer)

my_spdf <- readOGR(dsn= "complementary_data/geoportal_bcn/data/L%C3%ADmitdistricte.shp",verbose=FALSE)
#summary(my_spdf)
#length(my_spdf)
#head(my_spdf@data)

library(broom)
spdf_fortified <- tidy(my_spdf)
#head(spdf_fortified)

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
district_id_map <- fread("complementary_data/district_code.csv")
district_id_map[,id:=as.character(id)]
test_new_spdf <- merge(as.data.table(spdf_fortified),district_id_map,by="id")
## Adding labels
cnames_test <- aggregate(cbind(long, lat) ~ district_name, data=test_new_spdf, 
                         FUN=function(x)mean(range(x)))
ggplot() +
  geom_polygon(data = test_new_spdf, aes( x = long, y = lat, group = group,fill=district_name)) +
  geom_text(data=cnames_test, aes(long, lat, label = district_name), size=5)+
  scale_fill_brewer(palette = "Paired")+
  theme_bw()


## Adding tram data to map
tram_spdf <- merge(test_new_spdf, tram_count, by="district_name")

#Adding color
mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (20))) 
                      #, legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica"), 
                      #legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica"), 
                      #axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
                      #axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10)))

ggplot() +
  geom_polygon(data = tram_spdf, aes( x = long, y = lat, group = group,fill=N)) +
  geom_text(data=cnames_test, aes(long, lat, label = district_name), size=5)+
  theme_grey()+
  scale_fill_distiller(palette = "Spectral", direction=1,limits=c(0,16), breaks = c(0,4,8,12,16,20))+
  ggtitle("Number of tram stations per district")+
  mynamestheme
  
  
  

  # scale_fill_gradientn(colors='red')
  












