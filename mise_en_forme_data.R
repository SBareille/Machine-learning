library(spocc)
library(robis)
library(sp)
library(rgdal)
library(maptools)
library(raster)
library(SDMTools)
# les librairies suivantes ne sont pas forcemment utiles, � voir
library(gridExtra)
library(biomod2) 
library(PresenceAbsence)
library(rgeos)
library(plyr)
library(ecospat)
library(ade4)
library(rworldmap)
require(stringr)
library(colorRamps)
library(biogeonetworks) 

### Set working directory 
setwd("C:/Users/serva/Google Drive/1-Partage Ordis/M2/Machine_learning/Projet")


# Download occurrences from OBIS  
# on garde seulement les colonnes coordonn�es et date d'observation (pour pouvoir r�cup�rer les donn�es environnementales correspondantes)

OCC <- robis::occurrence(scientificname = "Chaetodon rainfordi" ) # Download, be carefull of the firewall
if (dim(OCC)[1] > 0) {
  OCC <- cbind(OCC[, c("decimalLongitude", "decimalLatitude", "date_year")])
  names(OCC) <- c("longitude", "latitude", "year")
}

# enlever les lignes o� l'on n'a pas de date d'observation, ou si le poisson a �t� observ� avant 1985
OCC$year <- na.omit(OCC$year)
OCC <- OCC[-which(OCC$year<1985),]

# enlever les poissons qui sont en dehors de notre environnement d'�tude et ceux qui ont longitudes et latitudes =0
# pas possible encore car on a besoin de manipuler ces jeux de donn�es avant. 
# zero.coord <- which(OCC$longitude == 0 & OCC$latitude == 0)
# OCC <- OCC[ -which(OCC$longitude == 0 & OCC$latitude == 0), ]

############# Cr�ation du date frame complet occurence ##############

# � partir des longitudes et latitudes des occurences, r�cup�rer les donn�es environnementales correspondantes et les ajouter au tableau

# associer pour chaque ann�e la decade correspondante pour pouvoir r�cup�rer temp�rature et salinit�
# il y a un probl�me -> le r�gler
OCC@data$decade <- ifelse(OCC@data$year >= 1985 & OCC@data$year <= 1994, 1, OCC@data$decade)
OCC@data$decade <- ifelse(OCC@data$year >= 1995 & OCC@data$year <= 2004, 2, OCC@data$decade)
OCC@data$decade <- ifelse(OCC@data$year >= 2004 & OCC@data$year <= 2012, 3, OCC@data$decade)
if(length(which(is.na(OCC@data$decade))) > 0){OCC = OCC[-which(is.na(OCC@data$decade)), ]}
OCC@data$Temperature <- NA
OCC@data$Salinity <- NA
#blabla


#Importing climatic data for each time period
# voir si on n'importe pas le jeu de donn�es directement de NOAH.
# Ici on a donn�es o� les temp�ratures sont tri�es en 3 cat�gories en fonction de la profondeur dans la mer. Est-ce utile ici ? faire nous m�me les profondeurs ? voir comment c'est pr�sent� sur NOAH
# se servir des profondeur d'observation des poissons (qu'on a jart� de notre jeu d'occurance) pour savoir exactement la temp� � ce point d'obs

download.file("https://pcsbox.univ-littoral.fr/d/b75dc393597748659c0f/files/?p=%2FEnvironmental%20data%2FClimatic_data%2FWOD%2FTemperature_1985_1994.RData&dl=1",
              destfile = "Temperature_1985_1994.RData", mode = "wb")
download.file("https://pcsbox.univ-littoral.fr/d/b75dc393597748659c0f/files/?p=%2FEnvironmental%20data%2FClimatic_data%2FWOD%2FTemperature_1995_2004.RData&dl=1",
              destfile = "Temperature_1995_2004.RData", mode = "wb")
download.file("https://pcsbox.univ-littoral.fr/d/b75dc393597748659c0f/files/?p=%2FEnvironmental%20data%2FClimatic_data%2FWOD%2FTemperature_2005_2012.RData&dl=1",
              destfile = "Temperature_2005_2012.RData", mode = "wb")

Temperature1 <- brick(get(load("Temperature_1985_1994.RData")))
Temperature2 <- brick(get(load("Temperature_1995_2004.RData")))
Temperature3 <- brick(get(load("Temperature_2005_2012.RData")))

spplot(Temperature1, names.attr=c("Bottom","0-50m","0-200m"), main="Temperature 1985_1994")

download.file("https://pcsbox.univ-littoral.fr/d/b75dc393597748659c0f/files/?p=%2FEnvironmental%20data%2FClimatic_data%2FWOD%2FSalinity_1985_1994.RData&dl=1",
              destfile = "Salinity_1985_1994.RData", mode = "wb")
download.file("https://pcsbox.univ-littoral.fr/d/b75dc393597748659c0f/files/?p=%2FEnvironmental%20data%2FClimatic_data%2FWOD%2FSalinity_1995_2004.RData&dl=1",
              destfile = "Salinity_1995_2004.RData", mode = "wb")
download.file("https://pcsbox.univ-littoral.fr/d/b75dc393597748659c0f/files/?p=%2FEnvironmental%20data%2FClimatic_data%2FWOD%2FSalinity_2005_2012.RData&dl=1",
              destfile = "Salinity_2005_2012.RData", mode = "wb")

Salinity1 <- brick(get(load("Salinity_1985_1994.RData")))
Salinity2 <- brick(get(load("Salinity_1995_2004.RData")))
Salinity3 <- brick(get(load("Salinity_2005_2012.RData")))
spplot(Salinity1,names.attr=c("Bottom","0-50m","0-200m"),main="Salinity 1985_1994")
```



############# Cr�ation du date frame complet absence ##############

# d�finir autour de nos coordonn�es d'occurence d'observation une zone dans laquelle il ne sera pas possible de piocher les absences
# � faire

# piocher, au sein de notre zone d'�tude (pour laquelle on a toutes les infos enviro), et en dehors de nos zones au voisinnage d'une pr�sence nouvellement d�limit�es des coordonn�es qui correspondront aux endroits d'"absence"
# piocher autant d'absences que d'occurences

# ajouter ensuite � partir de ce tableau ABS les colonnes avec les variables enviro correspondantes comme on l'a fait pour OCC