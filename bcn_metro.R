#Libraries
library(ggplot2)
library(data.table)
library(magrittr) 
library(ggrepel)
library(GGally)
library(tidyr)
library(pheatmap)
library(factoextra)
library(dplyr)

#Set working directory
setwd("/Users/franciscalagos/Library/Mobile Documents/com~apple~CloudDocs/Documents/TUM/1st Semester/DataViz/Case Study/")
#Read data bases
transports <- fread("data/transports.csv")
transports <- rename_with(transports, tolower)
transports <- rename(transports, "district_name" = "district.name")
transports <- rename(transports, "neighborhood_name" = "neighborhood.name")
#colnames(transports)

#Prepare the underground db
und <- transports[transport == "Underground"]
und$code <- NULL
und$transport <- NULL
und$longitude <- NULL
und$latitude <- NULL
und <- separate(und, col= station, into=c("line", "station_name"), remove = TRUE , sep = '- ')
#unique(und$station_name)
und <- separate(und, col=line, into=c("type", "line_number"), remove = TRUE , sep =' ')
und <- separate(und, col=station_name, into=c("station_name", "nothing"), remove = FALSE , sep="-")
und$nothing <- NULL
und$type <- NULL

#unique(und$line_number)
und[und$line_number=="(L1)", "line_number"] <- "L1"
und[und$line_number=="(L2)", "line_number"] <- "L2"
und[und$line_number=="(L3)", "line_number"] <- "L3"
und[und$line_number=="(L3,", "line_number"] <- "L3"
und[und$line_number=="(L4)", "line_number"] <- "L4"
und[und$line_number=="(L5)", "line_number"] <- "L5"
und[und$line_number=="(L6)", "line_number"] <- "L6"
und[und$line_number=="(L6)", "line_number"] <- "L6"
und[und$line_number=="(L7)", "line_number"] <- "L7"
und[und$line_number=="(L8)", "line_number"] <- "L8"
und[und$line_number=="(L9)", "line_number"] <- "L9"
und[und$line_number=="(L9,", "line_number"] <- "L9"
und[und$line_number=="(L10)", "line_number"] <- "L10"
und[und$line_number=="(L11)", "line_number"] <- "L11"


#DELETE ALL REPEATED STATIONS
und <- und[!duplicated(und), ]
und_complete <- fread("complementary_data/bcn_metro.csv")
und_left_merge <- merge(und_complete, und, by = "station_name", all.x = TRUE) 

#Delete repeated stations
und_left_merge <- und_left_merge[!(und_left_merge$line_number=="(L6,L7)" & und_left_merge$station_name=="CATALUNYA"),]
und_left_merge <- und_left_merge[!(und_left_merge$line_number=="L3" & und_left_merge$station_name=="CATALUNYA"),]
und_left_merge <- und_left_merge[!(und_left_merge$line_number=="L2" & und_left_merge$station_name=="CLOT"),]
und_left_merge <- und_left_merge[!(und_left_merge$line_number=="L3" & und_left_merge$station_name=="ESPANYA"),]
und_left_merge <- und_left_merge[!(und_left_merge$line_number=="L8" & und_left_merge$station_name=="ESPANYA"),]
und_left_merge <- und_left_merge[!(und_left_merge$line_number=="L2" & und_left_merge$station_name=="LA PAU"),]
und_left_merge <- und_left_merge[!(und_left_merge$line_number=="L5" & und_left_merge$station_name=="MARAGALL"),] 
und_left_merge <- und_left_merge[!(und_left_merge$neighborhood_name=="les Tres Torres" & und_left_merge$station_name=="SARRIÀ"),]
und_left_merge <- und_left_merge[!(und_left_merge$line_number=="L4" & und_left_merge$station_name=="URQUINAONA"),] 
und_left_merge <- und_left_merge[!(und_left_merge$line_number=="L5" & und_left_merge$station_name=="VERDAGUER"),]
und_left_merge <- und_left_merge[!(und_left_merge$line_number=="L2" & und_left_merge$station_name=="GORG"),] 
und_left_merge <- und_left_merge[!(und_left_merge$line_number=="L1" & und_left_merge$station_name=="LA SAGRERA"),]
und_left_merge <- und_left_merge[!(und_left_merge$line_number=="L9" & und_left_merge$station_name=="LA SAGRERA"),]
und_left_merge <- und_left_merge[!(und_left_merge$line_number=="L2" & und_left_merge$station_name=="PASSEIG DE GRÀCIA"),]
und_left_merge <- und_left_merge[!(und_left_merge$line_number=="L3" & und_left_merge$station_name=="PASSEIG DE GRÀCIA"),]
und_left_merge <- und_left_merge[!(und_left_merge$line_number=="L5" & und_left_merge$station_name=="PLAÇA DE SANTS"),]
und_left_merge <- und_left_merge[!(und_left_merge$line_number=="L5" & und_left_merge$station_name=="DIAGONAL"),]
und_left_merge <- und_left_merge[!(und_left_merge$line_number=="L5" & und_left_merge$station_name=="SANTS ESTACIÓ"),]
und_left_merge <- und_left_merge[!(und_left_merge$line_number=="L3" & und_left_merge$station_name=="TRINITAT NOVA"),]
und_left_merge <- und_left_merge[!(und_left_merge$line_number=="L1" & und_left_merge$station_name=="UNIVERSITAT"),]
und_left_merge$line_number <- NULL


#Complete missing districts/neighborhoods
und_left_merge[und_left_merge$station_name=="AVINGUDA TIBIDABO", "neighborhood_name"] <- "Sant Gervasi - la Bonanova"
und_left_merge[und_left_merge$station_name=="AVINGUDA TIBIDABO", "district_name"] <- "Sarrià-Sant Gervasi"
und_left_merge[und_left_merge$station_name=="PÀDUA", "neighborhood_name"] <- "Sant Gervasi - Galvany"
und_left_merge[und_left_merge$station_name=="PÀDUA", "district_name"] <- "Sarrià-Sant Gervasi" 
und_left_merge[und_left_merge$station_name=="	FABRA I PUÍG", "neighborhood_name"] <- "Sant Andreu"
und_left_merge[und_left_merge$station_name=="	FABRA I PUÍG", "district_name"] <- "Sant Andreu"
und_left_merge[und_left_merge$station_name=="MAGÒRIA-LA CAMPANA", "neighborhood_name"] <- "la Bordeta"
und_left_merge[und_left_merge$station_name=="MAGÒRIA-LA CAMPANA", "district_name"] <- "Sants-Montjuïc"
und_left_merge[und_left_merge$station_name=="PARAL-LEL", "neighborhood_name"] <- "el Poble-sec"
und_left_merge[und_left_merge$station_name=="PARAL-LEL", "district_name"] <- "Sants-Montjuïc"
und_left_merge[und_left_merge$station_name=="MARÍA CRISTINA", "neighborhood_name"] <- "les Corts"
und_left_merge[und_left_merge$station_name=="MARÍA CRISTINA", "district_name"] <- "Les Corts"
und_left_merge[und_left_merge$station_name=="SAGRADA FAMILIA", "neighborhood_name"] <- "la Sagrada Família"
und_left_merge[und_left_merge$station_name=="SAGRADA FAMILIA", "district_name"] <- "Eixample"
und_left_merge[und_left_merge$station_name=="SANT PAU / DOS DE MAIG", "neighborhood_name"] <- "la Sagrada Família"
und_left_merge[und_left_merge$station_name=="SANT PAU / DOS DE MAIG", "district_name"] <- "Eixample"
und_left_merge[und_left_merge$station_name=="GLORIES", "neighborhood_name"] <- "el Parc i la Llacuna del Poblenou"
und_left_merge[und_left_merge$station_name=="GLORIES", "district_name"] <- "Sant Martí"
und_left_merge[und_left_merge$station_name=="EL MARESME / FÒRUM", "neighborhood_name"] <- "el Besòs i el Maresme"
und_left_merge[und_left_merge$station_name=="EL MARESME / FÒRUM", "district_name"] <- "Sant Martí"
und_left_merge[und_left_merge$station_name=="CIUDADELLA / VILLA OLIMPICA", "neighborhood_name"] <- "la Vila Olímpica del Poblenou"
und_left_merge[und_left_merge$station_name=="CIUDADELLA / VILLA OLIMPICA", "district_name"] <- "Sant Martí"
und_left_merge[und_left_merge$station_name=="CAMP DE L´ARPA", "neighborhood_name"] <- "el Camp de l'Arpa del Clot"
und_left_merge[und_left_merge$station_name=="CAMP DE L´ARPA", "district_name"] <- "Sant Martí"
und_left_merge[und_left_merge$station_name=="FABRA I PUÍG", "neighborhood_name"] <- "Sant Andreu"
und_left_merge[und_left_merge$station_name=="FABRA I PUÍG", "district_name"] <- "Sant Andreu"
und_left_merge[und_left_merge$station_name=="GUÍNARDÓ / HOSPITAL DE SANT PAU", "neighborhood_name"] <- "el Guinardó"
und_left_merge[und_left_merge$station_name=="GUÍNARDÓ / HOSPITAL DE SANT PAU", "district_name"] <- "Horta-Guinardó"
und_left_merge[und_left_merge$station_name=="VÍA JÚLIA", "neighborhood_name"] <- "la Prosperitat"
und_left_merge[und_left_merge$station_name=="VÍA JÚLIA", "district_name"] <- "Nou Barris"
und_left_merge[und_left_merge$station_name=="MARÍNA", "neighborhood_name"] <- "el Parc i la Llacuna del Poblenou"
und_left_merge[und_left_merge$station_name=="MARÍNA", "district_name"] <- "Sant Martí"

#Complete stations with more than one District/Neighborhood
new_row_1 <- data.frame("ESPANYA", "L1,L3,L8","Eixample", "Sant Antoni")
names(new_row_1) <- c("station_name", "line", "district_name", "neighborhood_name")
new_row_2 <- data.frame("TARRAGONA", "L3","Sants-Montjuïc", "Hostafrancs")
names(new_row_2) <- c("station_name", "line", "district_name", "neighborhood_name")
new_row_3 <- data.frame("PARAL-LEL", "L2,L2", "Ciutat Vella", "el Raval")
names(new_row_3) <- c("station_name", "line", "district_name", "neighborhood_name")
new_row_4 <- data.frame("PARAL-LEL", "L2,L2", "Eixample", "Sant Antoni")
names(new_row_4) <- c("station_name", "line", "district_name", "neighborhood_name")
new_row_5 <- data.frame("POBLE SEC", "L3", "Sants-Montjuïc", "el Poble-sec")
names(new_row_5) <- c("station_name", "line", "district_name", "neighborhood_name")
new_row_6 <- data.frame("UNIVERSITAT", "L1,L2", "Ciutat Vella", "el Raval")
names(new_row_6) <- c("station_name", "line", "district_name", "neighborhood_name")
new_row_7 <- data.frame("SANT ANTONI", "L2", "Ciutat Vella", "el Raval")
names(new_row_7) <- c("station_name", "line", "district_name", "neighborhood_name")
new_row_8 <- data.frame("CATALUNYA", "L1,L3,L6,L7", "Ciutat Vella", "el Raval")
names(new_row_8) <- c("station_name", "line", "district_name", "neighborhood_name")
new_row_9 <- data.frame("URQUINAONA", "L1,L4", "Ciutat Vella", "Sant Pere, Santa Caterina i la Ribera")
names(new_row_9) <- c("station_name", "line", "district_name", "neighborhood_name")
new_row_10 <- data.frame("GRÀCIA", "L6,L7", "Sarrià-Sant Gervasi", "Sant Gervasi - Galvany")
names(new_row_10) <- c("station_name", "line", "district_name", "neighborhood_name")
new_row_11 <- data.frame("LESSEPS", "L3", "Sarrià-Sant Gervasi", "el Putxet i el Farró")
names(new_row_11) <- c("station_name", "line", "district_name", "neighborhood_name")
new_row_12 <- data.frame("MARAGALL", "L4,L5", "Nou Barris", "Vilapicina i la Torre Llobeta")
names(new_row_12) <- c("station_name", "line", "district_name", "neighborhood_name")
new_row_13 <- data.frame("MARAGALL", "L4,L5", "Horta-Guinardó", "el Guinardó")
names(new_row_13) <- c("station_name", "line", "district_name", "neighborhood_name")
new_row_14 <- data.frame("EL COLL / LA TEIXONERA", "L5", "Gràcia", "el Coll")
names(new_row_14) <- c("station_name", "line", "district_name", "neighborhood_name")

und_final <- rbind(und_left_merge, new_row_1, new_row_2, new_row_3, new_row_4, new_row_5, new_row_6, new_row_7, new_row_8, new_row_9,new_row_10, new_row_11, new_row_12,new_row_13, new_row_14)

und_count <- und_final[,.N,by='district_name']
und_count<- und_count[!(und_count$N==28),] 
und_count<- und_count[!(und_count$N==27),] 

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

## District to id map
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


## Adding underground data to map
und_spdf <- merge(test_new_spdf, und_count, by='district_name')
mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (20))) 

ggplot() +
  geom_polygon(data = und_spdf, aes( x = long, y = lat, group = group,fill=N)) +
  geom_text(data=cnames_test, aes(long, lat, label = district_name), size=5)+
  # scale_fill_gradientn(colors='red')+
  scale_fill_distiller(palette = "BuPu", direction=1,limits=c(5,22), breaks = c(5,10,15,20,25))+
  ggtitle("Number of metro stations per district")+
  mynamestheme








