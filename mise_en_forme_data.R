library(spocc)
library(scales)
library(robis)
library(sp)
library(rgdal)
library(maptools)
library(raster)
library(SDMTools)
#library(openxlsx)
# les librairies suivantes ne sont pas forcemment utiles, à voir
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
# on garde seulement les colonnes coordonnées et date d'observation (pour pouvoir récupérer les données environnementales correspondantes)

OCC <- robis::occurrence(scientificname = "Chaetodon rainfordi" ) # Download, be carefull of the firewall
if (dim(OCC)[1] > 0) {
  OCC <- cbind(OCC[, c("decimalLongitude", "decimalLatitude", "date_year")])
  names(OCC) <- c("longitude", "latitude", "year")
}


# enlever les lignes où l'on n'a pas de date d'observation, ou si le poisson a été observé avant 1985 (car pas assez d'observations par decades)
OCC <- na.omit(OCC, cols='year')
OCC <- OCC[-which(OCC$year<1985),]
OCC <- OCC[-which(OCC$year>2012),] # car on n'a pas de températures dispo après 2012. Même sur NOAA. 
                                  # doit on vraiment enlever ces poissons ? car il y en a quand même environ 900 (sur environ 4900)
# associer pour chaque année la decade correspondante pour pouvoir récupérer température et salinité
OCC$decade <- NA
OCC$decade <- ifelse(OCC$year >= 1985 & OCC$year <= 1994, 1, OCC$decade)
OCC$decade <- ifelse(OCC$year >= 1995 & OCC$year <= 2004, 2, OCC$decade)
OCC$decade <- ifelse(OCC$year >= 2004 & OCC$year <= 2012, 3, OCC$decade)

#Plot occurrences
# a faire. les lignes en dessous ne sont que des exemples
# plot(getMap(resolution = "coarse"),col="gray")
# plot(OCC,add=T,col=2,cex=0.3,pch=15)

# enlever les poissons qui sont en dehors de notre environnement d'étude et ceux qui ont longitudes et latitudes =0
# pas possible encore car on a besoin de manipuler ces jeux de données avant.
# on va plutot de récupérer les infos geometric pour nos coord d'occurences, et si c'est pas possible c'est qu'on n'est pas dans la zone (supprimer cette occurence)


##### on va maintenant créer la base de notre dataframe absences
# on ajoutera les colonnes variables pour chaque data frame en parallèle ensuite
# mais pour créer les absences il faut rester dans notre zone d'étude
# pour cela on va se servir des infos locales comme les données géomorphiques de la barrière du corail


# Importing geomorphic features
# Ce csv a été créé à partir des données sur https://www.deepreef.org/bathymetry/65-3dgbr-bathy.html 
# pour chaque fichier shapefile les commandes suivantes ont été effectuées, puis tout a été fusionné : 
# my_spdf_cay <- readOGR( 
#   dsn= "3dgbr_geomorph/shape", 
#   layer = "coralsea_cay",
#   verbose=FALSE)
# cc = coordinates(my_spdf_cay)
# geomorphic_1 = data.frame(cc)
# colnames(geomorphic_1) = c("lon","lat")
# geomorphic_1$type = "cay"

download.file("http://sdm.dev-lab.net/geomorphic.csv",
              destfile = "geomorphic.csv", mode = "wb")
geomorphic <- read.table("geomorphic.csv", sep = ',', header = T)


# faire représentation graphique ? 
# Exemple: on trace les coordonnÃ©es des points du canyon
#plot(coordinates(my_spdf_canyon),pch=20)
# On ajoute le plateau
#points(coordinates(my_spdf_plateau),pch=17,col="orange")
# Faire un plot représentant tout le geomorphic ??
# plot(coordinate(geomorphic))

# Grace aux coordonnées de notre zone où a des informations sur le geomorph on a l'étendu de notre zone d'étude
# Avant de créer les absences, on va enlever les poissons qui ont été observés trop loins de notre zone d'étude en se servant de la distance minimiale qu'ils ont avec celle-ci
# si à plus de 100 km de la zone on supprime.
for (k in 1:nrow(OCC)){
  d = sqrt((OCC$longitude[k]-geomorphic$lon)^2 + (OCC$latitude[k]-geomorphic$lat)^2)
  if (min(d) > 100000/(100000 * 1.04)){
    OCC <- OCC[-k,]
    }
}
# le problème c'est qu'il y a beaucoup de données qui sont à plus de 100 km d'une donnée locale geomorphic
# en fait on n'a pas assez de données geomorphic pour s'en servir pour délimiter la zone
# il faudrait qu'on se serve des infos de boundaries mais comme il ne s'agit pas d'une zone rectangulaire on n'arrive pas faire des conditions sur ces limites


# # Importing boudries coordinates 

# boundaries <- readOGR( 
#   dsn= "3dgbr_geomorph/shape", 
#   layer = "qld_gbrwha_cscz",
#   verbose=FALSE)
# 
# boundaries <- coordinates(boundaries)[[1]][[1]]
# plot(boundaries,pch=20)
# 
# # ajouter la limite sud (à latitude = -24.5)
# for (longit in seq(152.042,154,by=0.001)){
#   boundaries <- rbind(boundaries, c(longit,-24.5))
# }
# plot(boundaries,pch=20)


############# Création du date frame absence ##############

# définir autour de nos coordonnées d'occurence d'observation une zone dans laquelle il ne sera pas possible de piocher les absences


# ATTENTION !! Il faut qu'on reparte sur autre chose que juste les coords de geomorphic parce qu'il n'y a pas assez de couples de coordonnées dispos ! juste 1059 !
# Se servir de boundaries
# quand on créera le tableau ABS il faudra créer les mêmes colonnes que OCC pour pouvoir les fusionner ensuite





# les coordonnees sont des degres d嶰imaux. 
# une variation � partir du 5eme chiffre apres la virgule correspond a une variation d'environ 1.11 m a l'equateur.
# dans un soucis de simplification, et de part la petitesse de la superficie des zones calculees autour des occurrences,
# nous allons negliger la courbure de la Terre et considerer une variation de 1.11 m dans la Grande Barriere de corail.

# Rayon de la Terre : 6400 km
# La Grande Barri鋨e de corail se situe environ a 20 degres Sud
# Rayon de la Terre dans le plan passant par la latitude 20 degres Sud et parallele au plan de l'equateur : 6400 * cos(20) = 6014 km
# Le rapport entre la variation en metres entre la latitude a l'equateur et a la latitude de laGrande Barriere de corail est : 6014/6400 = 0.93969
# La variation au niveau de la Grande Barriere de corail est donc de 1.11 * 0.93969 = 1.04

lim = 2000/(100000 * 1.04) #on d嶨init les limites des zones o� l'on ne fera pas les tirages des pseudos absences avec un rayon de 2000 m autour de l'occurrence, que l'on convertit en variation de degres decimaux. 
#cette zone represente l'aire ou les poissons n'ont pas etes observes mais sont consideres comme existants

OCCd1 <- OCC[which(OCC$decade == 1),] # on separe chaque decennie
OCCd2 <- OCC[which(OCC$decade == 2),]
OCCd3 <- OCC[which(OCC$decade == 3),]

T1<-Sys.time()

geomd1 <- geomorphic
for (i in 1:nrow(OCCd1)){# pour chaque occurrence
  for(j in 1:nrow(geomorphic)){
    if(sqrt((OCCd1[i,1] - geomorphic[j,1])^2 + (OCCd1[i,2] - geomorphic[j,2])^2) <= lim){# si les coordonnees utilisees plus tard pour generer les pseudo absences sont dans la zone, alors la donnee correspondante est supprimee
      geomd1 <- geomd1[ -j,]
    }
  }
}

T2<-Sys.time()
Tdiff= difftime(T2, T1) 

geomd2 <- geomorphic
for (i in 1:nrow(OCCd2)){
  for(j in 1:nrow(geomorphic)){
    if(sqrt((OCCd2[i,1] - geomorphic[j,1])^2 + (OCCd2[i,2] - geomorphic[j,2])^2) <= lim){     
      geomd2 <- geomd2[ -j,]
    }
  }
}

geomd3 <- geomorphic
for (i in 1:nrow(OCCd3)){
  for(j in 1:nrow(geomorphic)){
    if(sqrt((OCCd3[i,1] - geomorphic[j,1])^2 + (OCCd3[i,2] - geomorphic[j,2])^2) <= lim){      
      geomd3 <- geomd1[ -j,]
    }
  }
}

par(mfrow = c(3,1))
# plot separe pour chaque decennie (uniquement visualisation, soit pimper soit tej)

plot(x=geomorphic[,1], y=geomorphic[,2], xlim=c(144,160), ylim=c(-25,-10), col='red')
par(new=T)
plot(x=OCCd1[,1], y=OCCd1[,2], xlim=c(144,160), ylim=c(-25,-10), col='green')


plot(x=geomorphic[,1], y=geomorphic[,2], xlim=c(144,160), ylim=c(-25,-10), col='red')
par(new=T)
plot(x=OCCd2[,1], y=OCCd2[,2], xlim=c(144,160), ylim=c(-25,-10), col='green')


plot(x=geomorphic[,1], y=geomorphic[,2], xlim=c(144,160), ylim=c(-25,-10), col='red')
par(new=T)
plot(x=OCCd3[,1], y=OCCd3[,2], xlim=c(144,160), ylim=c(-25,-10), col='green')


# piocher, au sein de notre zone d'矇tude (pour laquelle on a toutes les infos enviro), et en dehors de nos zones au voisinage d'une pr矇sence nouvellement d矇limit矇es des coordonn矇es qui correspondront aux endroits d'"absence"
# Les tirages sont différents pour chaque décennie pour pouvoir prendre en compte un changement de distribution si il y en a eu.  
# piocher autant d'absences que d'occurences. 

ABSd1 <- geomd1 #initialisation du dataframe contenant les pseudo absences pour la decennie 1 (puis pour chaque decennie apres)

for (i in 1:nrow(ABSd1)){ # on vide les lignes (on veut un tableau vide a remplir au fur et a mesure)
  ABSd1[i,] = NA
}
set.seed(1)
for (i in 1:nrow(OCCd1)){ # chaque ligne devient un tirage aleatoire parmi les donnees du sol de la Grande Barriere de corail
  ABSd1[i,] = geomd1[sample(1:nrow(geomd1)),]
}

ABSd2 <- geomd2

for (i in 1:nrow(ABSd2)){
  ABSd2[i,] = NA
}
set.seed(1)
for (i in 1:nrow(OCCd2)){
  ABSd2[i,] = geomd2[sample(1:nrow(geomd2)),]
}

ABSd3 <- geomd3

for (i in 1:nrow(ABSd3)){
  ABSd3[i,] = NA
}
set.seed(1)
for (i in 1:nrow(OCCd3)){
  ABSd3[i,] = geomd2[sample(1:nrow(geomd3)),]
}

ABS <- rbind(ABSd1, ABSd2, ABSd3)

# on ajoute une colonne presence remplie de 0 ou 1 pour pouvoir ensuite lors de notre analyse assimiler que 1=présence et 0=absence
OCC$presence <- NA
OCC$presence <- rep(1, nrow(OCC))
ABS$presence <- NA
ABS$presence <- rep(0, nrow(ABS))
ABS$year <- NA  # pour avoir le même nombre de colonnes qu'avec OCC pour fusion
# ajouter colonne year aussi
# le problème c'est que les noms de colonnes sont différents. A voir si on fait un tirage au sort à partir d'autre chose que geomorphic (à partir des coordonnées comprises dans boundaries)

# fusionner les deux tableaux OCC et ABS pour ensuite analyser facilement les données
# la fusion avant le récupérage des variables permet d'écrire moins de lignes de code, mais si besoin on peut aussi récupérer les variables pour chaque tableau séparément.

DATA <- rbind(OCC, ABS)
# ainsi PA$resp   qu'on a dans le script de l'article sera  DATA$presence   


############# Ajout des variables pour nos données occurences et absences à partir des coordonnées  ##############

# On commence par ajouter la variable geomorphic (comme on l'a déjà chargé)

for (k in 1:nrow(DATA)){
  d = sqrt((DATA$longitude[k]-geomorphic$lon)^2 + (DATA$latitude[k]-geomorphic$lat)^2) 
  i_dmin = which.min(d)
  DATA$geomorphic[k] = geomorphic$type[i_dmin]
}

# Importing climatic data for each time period (température et salinité)
# voir si on n'importe pas le jeu de données directement de NOAH.
# Ici on a données où les températures sont triées en 3 catégories en fonction de la profondeur dans la mer. Est-ce utile ici ? faire nous même les profondeurs ? voir comment c'est présenté sur NOAH
# se servir des profondeur d'observation des poissons (qu'on a jarté de notre jeu d'occurance) pour savoir exactement la tempé à ce point d'obs

download.file("http://sdm.dev-lab.net/Temperature_1985_1994.RData",
              destfile = "Temperature_1985_1994.RData", mode = "wb")
download.file("http://sdm.dev-lab.net/Salinity_1985_1994.RData",
              destfile = "Salinity_1985_1994.RData", mode = "wb")
Temperature1 <- brick(get(load("Temperature_1985_1994.RData")))
Salinity1 <- brick(get(load("Salinity_1985_1994.RData")))
spplot(Salinity1,names.attr=c("Bottom","0-50m","0-200m"),main="Salinity 1985_1994")
spplot(Temperature1, names.attr=c("Bottom","0-50m","0-200m"), main="Temperature 1985_1994")

# c'est les large spatialpixel data frame Temperature et Salinity crée en même temps qui nous interessent. 
# le problème c'est qu'à chaque fois qu'on charge un nouveau raster température (ou salinité) celui là va être changer
# Ainsi on récupère les données de temperatures et salinité qui nous interessent pour ce decade (pour data frame OCC et ABS), puis on charge un nouveau jeu de données températures (en salinité)
# les coordonnées des data frame température et salinity sont les mêmes (même pas de données originelle NOAA). Ainsi on peut ne chercher la coord la plus proche qu'une fois
for (k in 1:nrow(DATA)){
  if (DATA$decade[k] == 1){
    d = sqrt((DATA$longitude[k]-Temperature@coords[,1])^2+ (DATA$latitude[k]-Temperature@coords[,2])^2) 
    i_dmin = which.min(d)
    DATA$temperature[k] = Temperature$Surface_mean[i_dmin]
    DATA$salinity[k] = Salinity$Surface_mean[i_dmin]
  }
}

download.file("http://sdm.dev-lab.net/Temperature_1995_2004.RData",
              destfile = "Temperature_1995_2004.RData", mode = "wb")
download.file("http://sdm.dev-lab.net//Salinity_1995_2004.RData",
              destfile = "Salinity_1995_2004.RData", mode = "wb")
Temperature2 <- brick(get(load("Temperature_1995_2004.RData")))
Salinity2 <- brick(get(load("Salinity_1995_2004.RData")))
for (k in 1:nrow(DATA)){
  if (DATA$decade[k] == 2){
    d = sqrt((DATA$longitude[k]-Temperature@coords[,1])^2+ (DATA$latitude[k]-Temperature@coords[,2])^2) 
    i_dmin = which.min(d)
    DATA$temperature[k] = Temperature$Surface_mean[i_dmin]
    DATA$salinity[k] = Salinity$Surface_mean[i_dmin]
  }
}


download.file("http://sdm.dev-lab.net//Temperature_2005_2012.RData",
              destfile = "Temperature_2005_2012.RData", mode = "wb")
download.file("http://sdm.dev-lab.netSalinity_2005_2012.RData",
              destfile = "Salinity_2005_2012.RData", mode = "wb")
Temperature3 <- brick(get(load("Temperature_2005_2012.RData")))
Salinity3 <- brick(get(load("Salinity_2005_2012.RData")))
for (k in 1:nrow(DATA)){
  if (DATA$decade[k] == 3){
    d = sqrt((DATA$longitude[k]-Temperature@coords[,1])^2+ (DATA$latitude[k]-Temperature@coords[,2])^2) 
    i_dmin = which.min(d)
    DATA$temperature[k] = Temperature$Surface_mean[i_dmin]
    DATA$salinity[k] = Salinity$Surface_mean[i_dmin]
  }
}

download.file("http://sdm.dev-lab.net/sampled_bathymetry.csv",
              destfile = "sampled_bathymetry.csv", mode = "wb")
Bathymetry <- read.table("sampled_bathymetry.csv", sep = ',', header = T)
for (k in 1:nrow(DATA)){
  if (DATA$decade[k] == 3){
    d = sqrt((DATA$longitude[k]-Temperature@coords[,1])^2+ (DATA$latitude[k]-Temperature@coords[,2])^2) 
    i_dmin = which.min(d)
    DATA$temperature[k] = Temperature$Surface_mean[i_dmin]
    DATA$salinity[k] = Salinity$Surface_mean[i_dmin]
  }
}

# On réfléchit à ajouter des données sur les sédiments venant de MARS  http://dbforms.ga.gov.au/pls/www/npm.mars.search
# mais il faut qu'on étudie plus cet aspect et qu'on décide quelle forme de data on veut (récupérer que des sédiments à la surface)


# Avant analyse il faudra certainement "neutraliser" les coordonnées pour ne pas qu'elles soient considérées comme une variable explicative
DATA$longitude <- 1:nrow(DATA)
DATA$latitude <- 1:nrow(DATA) 

# Le tableau est prêt à être analyser ! 
