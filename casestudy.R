#Case Study
library(ggplot2)
library(data.table)
library(magrittr) # Needed for %>% operator library(tidyr)
library(ggrepel)
library(GGally)
library(tidyr)
library(pheatmap)
library(factoextra)
library(dplyr)

#Set working directory
setwd("/Users/franciscalagos/Library/Mobile Documents/com~apple~CloudDocs/Documents/TUM/1st Semester/DataViz/Case Study/data")
#Read data bases
accidents <- fread("accidents_2017.csv")
air_quality <- fread("air_quality_Nov2017.csv")
air_stations <- fread("air_stations_Nov2017.csv")
births <- fread("births.csv")
bus_stops <- fread("bus_stops.csv")
deaths <- fread("deaths.csv")
immigrants_nationality <- fread("immigrants_by_nationality.csv")
immigrants_emigrants_age <- fread("immigrants_emigrants_by_age.csv") 
immigrants_emigrants_destination <- fread("immigrants_emigrants_by_destination.csv") 
immigrants_emigrants_destination2 <- fread("immigrants_emigrants_by_destination2.csv") 
life_expectancy <- fread("life_expectancy.csv")
most_frequent_baby_names <- fread("most_frequent_baby_names.csv")
most_frequent_names <- fread("most_frequent_names.csv")
population <- fread("population.csv")
transports <- fread("transports.csv")
unemployment <- fread("unemployment.csv")

#Rename all the columns in a uniform way
accidents <- rename_with(accidents, tolower)
colnames(accidents) <- gsub(" ", "_", colnames(accidents))
colnames(accidents)

air_quality <- rename_with(air_quality, tolower)
colnames(air_quality) <- gsub(" ", "_", colnames(air_quality))
colnames(air_quality)

air_stations <- rename_with(air_stations, tolower)
colnames(air_stations) <- gsub(" ", "_", colnames(air_stations))
colnames(air_stations)

births <- rename_with(births, tolower)
colnames(births) <- gsub(" ", "_", colnames(births))
colnames(births)

bus_stops <- rename_with(bus_stops, tolower)
bus_stops <- rename(bus_stops, "district_name" = "district.name")
bus_stops <- rename(bus_stops, "neighborhood_name" = "neighborhood.name")
bus_stops <- rename(bus_stops, "bus_stop" = "bus.stop")
colnames(bus_stops)

deaths <- rename_with(deaths, tolower)
deaths <- rename(deaths, "district_code" = "district.code")
deaths <- rename(deaths, "district_name" = "district.name")
deaths <- rename(deaths, "neighborhood_code" = "neighborhood.code")
deaths <- rename(deaths, "neighborhood_name" = "neighborhood.name")
colnames(deaths)

immigrants_nationality <- rename_with(immigrants_nationality, tolower)
colnames(immigrants_nationality) <- gsub(" ", "_", colnames(immigrants_nationality))
colnames(immigrants_nationality)

immigrants_emigrants_age <- rename_with(immigrants_emigrants_age, tolower)
colnames(immigrants_emigrants_age) <- gsub(" ", "_", colnames(immigrants_emigrants_age))
colnames(immigrants_emigrants_age)

colnames(immigrants_emigrants_destination)
colnames(immigrants_emigrants_destination2)

life_expectancy <- rename_with(life_expectancy, tolower)
colnames(life_expectancy) <- gsub("-", "_", colnames(life_expectancy))
colnames(life_expectancy)

most_frequent_baby_names <- rename_with(most_frequent_baby_names, tolower)
colnames(most_frequent_baby_names)

most_frequent_names <- rename_with(most_frequent_names, tolower)
colnames(most_frequent_names)

population <- rename_with(population, tolower)
population <- rename(population, "district_code" = "district.code")
population <- rename(population, "district_name" = "district.name")
population <- rename(population, "neighborhood_code" = "neighborhood.code")
population <- rename(population, "neighborhood_name" = "neighborhood.name")
colnames(population)

transports <- rename_with(transports, tolower)
transports <- rename(transports, "district_name" = "district.name")
transports <- rename(transports, "neighborhood_name" = "neighborhood.name")
colnames(transports)

unemployment <- rename_with(unemployment, tolower)
colnames(unemployment) <- gsub(" ", "_", colnames(unemployment))
colnames(unemployment)

#Checking that every neighborhood is correctly assigned to it´s District
#cv <- transports[district_name == "Ciutat Vella", unique(neighborhood_name)]
#length(cv)
#le <- transports[district_name == "Eixample", unique(neighborhood_name)]
#length(le)
#s_m <- transports[district_name == "Sants-Montjuïc", unique(neighborhood_name)]
#length(s_m)
#lc <- transports[district_name == "Les Corts", unique(neighborhood_name)]
#length(lc)
#ssg <- transports[district_name == "Sarrià-Sant Gervasi", unique(neighborhood_name)]
#length(ssg)
#gr <- transports[district_name == "Gràcia", unique(neighborhood_name)]
#length(gr)
#hg <- transports[district_name == "Horta-Guinardó", unique(neighborhood_name)]
#length(hg)
#nb <- transports[district_name == "Nou Barris", unique(neighborhood_name)]
#length(nb)
#sa <- transports[district_name == "Sant Andreu", unique(neighborhood_name)]
#length(sa)
#sm <- transports[district_name == "Sant Martí", unique(neighborhood_name)]
#length(sm)

#Checking bus_stop
#cv <- bus_stops[district_name == "Ciutat Vella", unique(neighborhood_name)]
#length(cv)
#le <- bus_stops[district_name == "Eixample", unique(neighborhood_name)]
#length(le)
#s_m <- bus_stops[district_name == "Sants-Montjuïc", unique(neighborhood_name)]
#length(s_m)
#lc <- bus_stops[district_name == "Les Corts", unique(neighborhood_name)]
#length(lc)
#ssg <- bus_stops[district_name == "Sarrià-Sant Gervasi", unique(neighborhood_name)]
#length(ssg)
#gr <- bus_stops[district_name == "Gràcia", unique(neighborhood_name)]
#length(gr)
#hg <- bus_stops[district_name == "Horta-Guinardó", unique(neighborhood_name)]
#length(hg)
#nb <- bus_stops[district_name == "Nou Barris", unique(neighborhood_name)]
#length(nb)
#sa <- bus_stops[district_name == "Sant Andreu", unique(neighborhood_name)]
#length(sa)
#sm <- bus_stops[district_name == "Sant Martí", unique(neighborhood_name)]
#length(sm)





