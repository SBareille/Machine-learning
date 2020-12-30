library(spocc)
library(scales)
library(robis)
library(sp)
library(rgdal)
library(maptools)
library(raster)
library(SDMTools)
#library(openxlsx)
# les librairies suivantes ne sont pas forcemment utiles, Ã  voir
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
library(sf)


##################################################
### Importation et selection du jeu de donnees
##################################################


# telechargement des occurrences depuis OBIS 
# On garde seulement les colonnes coordonnees et date d'observation (pour pouvoir recuperer les donnees environnementales correspondantes)

OCC <- robis::occurrence(scientificname = "Chaetodon rainfordi" ) # Download, be carefull of the firewall
if (dim(OCC)[1] > 0) {
  OCC <- cbind(OCC[, c("decimalLongitude", "decimalLatitude", "date_year")])
  names(OCC) <- c("longitude", "latitude", "year")
}

# On supprime les occurrences ou l'on n'a pas de date d'observation, ou si le poisson a ete observe avant 1985 (car pas assez d'observations par decennie)
OCC <- na.omit(OCC, cols='year')
OCC <- OCC[-which(OCC$year<1985),]
OCC <- OCC[-which(OCC$year>2012),] # car on n'a pas de temperatures disponibles apres 2012. Meme sur NOAA. 
# doit on vraiment enlever ces poissons ? car il y en a quand meme environ 900 (sur environ 4900)


# Regroupement des occurrences de chaque annee par decennie correspondante
OCC$decade <- NA
OCC$decade <- ifelse(OCC$year >= 1985 & OCC$year <= 1994, 1, OCC$decade)
OCC$decade <- ifelse(OCC$year >= 1995 & OCC$year <= 2004, 2, OCC$decade)
OCC$decade <- ifelse(OCC$year >= 2004 & OCC$year <= 2012, 3, OCC$decade)

# Importation des limites des coordonnees
# Ces limites correspondent a la zone d'etude, pour laquelle nous possedons les informations geomorphiques
boundaries <- readOGR( 
  dsn= "3dgbr_geomorph/shape", 
  layer = "qld_gbrwha_cscz",
  verbose=FALSE)

# On regarde graphiquement si certaines occurrences se situent en dehors de la zone d'etude
plot(OCC$longitude, OCC$latitude, xlim=c(120,155), ylim=c(-28,-14))
par(new=T)
plot(coordinates(boundaries)[[1]][[1]],pch=20, xlim=c(120,155), ylim=c(-28,-14))

# Certaines occurrences sont en dehors des limites : elles sont alors supprimees.
OCC <- OCC[-which(OCC$longitude<140),]
OCC <- OCC[-which(OCC$latitude< -25),]

plot(OCC$longitude, OCC$latitude, xlim=c(120,155), ylim=c(-28,-14))
par(new=T)
plot(coordinates(boundaries)[[1]][[1]],pch=20, xlim=c(120,155), ylim=c(-28,-14))

# Toutes les occurrences restantes sont maintenant dans la zone d'etude.
# Les donnees sont mises en forme pour pouvoir etre manipulees.


##############################################################################################
### Mise en forme du jeu de donnees pour la creation de la zone de tirage des pseudos absences
##############################################################################################

# Les occurrences sont regroupes par decennie, pour permmettre une correspondance avec d'autres jeux de donnees 
# (temperature...) ou les releves ont ete enregistres par decennie.


### Separation des occurrences par decennie d'etude
OCCd1 <- OCC[which(OCC$decade == 1),]
OCCd2 <- OCC[which(OCC$decade == 2),]
OCCd3 <- OCC[which(OCC$decade == 3),]

# Exploration graphique des coordonnees des occurrences des 3 decennies
plot(OCCd1$longitude, OCCd1$latitude, xlim=c(140,160), ylim=c(-25,-10), col='black')
par(new=T)
plot(OCCd2$longitude, OCCd2$latitude, xlim=c(140,160), ylim=c(-25,-10), col='blue')
par(new=T)
plot(OCCd3$longitude, OCCd3$latitude, xlim=c(140,160), ylim=c(-25,-10), col='red')

# Lorsqu'il y a de nouvelles occurrences pour une decennie donnee compare a la (ou les) decennie(s) precedente(s),
# on remarque les les coordonnees des nouvelles occurrences ne sont pas tres eloignes geographiquement de celles deja releves.
# De plus, on remarque que la zone geographique la plus etendue ou il y a eu des relevees (occurrences) correspond a la 3eme decennie.
# Nous allons donc considerer la zone de tirage des pseudos absences dans une zone ou il n'y a pas eu d'occurrences relevee 
# au cours de la 3eme decennie, dans les limites de la zone d'etude.



# Initialisation des coordonnees de la zone d'etude
coord_bnd <- coordinates(boundaries)[[1]][[1]]
colnames(coord_bnd) <- c('longitude','latitude')
coord_bnd_d3 <-as.data.frame(coord_bnd)


# Initialisation des matrices contenants les pseudos absences par decennie
absd1 <- matrix(as.numeric(NA), nrow = nrow(OCCd1), ncol = 2)
absd2 <- matrix(as.numeric(NA), nrow = nrow(OCCd2), ncol = 2)
absd3 <- matrix(as.numeric(NA), nrow = nrow(OCCd3), ncol = 2)


# on etudie les occurrences des poissons selon leurs coordonnees, puis on decide de definir la zone
# ou il n'y aura que des presences (aucune pseudo absence)




#######################################################################################################
### Definition de la distance minimale entre les occurrences et la zone de tirage des pseudos absences
#######################################################################################################


# Les coordonnees sont des degres decimaux. 
# Une variation a partir du 5eme chiffre apres la virgule correspond a une variation d'environ 1.11 m a l'equateur.
# Dans un soucis de simplification, nous allons negliger la courbure de la Terre et considerer une variation constante 
# dans la Grande Barriere de corail.

# Rayon de la Terre : 6400 km
# La Grande Barriere de corail se situe environ a 20 degres Sud
# Rayon de la Terre dans le plan passant par la latitude 20 degres Sud et parallele au plan de l'equateur : 6400 * cos(20) = 6014 km
# Le rapport entre la variation en metres entre la latitude a l'equateur et la latitude de la Grande Barriere de corail est : 6014/6400 = 0.93969
# La variation au niveau de la Grande Barriere de corail est donc de 1.11 * 0.93969 = 1.04

lim = 1000/(100000 * 1.04) #on definit la distance minimale entre les occurrences et la zone de tirage des pseudos absences


#########################################################
### Definition de la zone de tirage des pseudos absences
#########################################################


# Pour simplifier le probleme, et avoir des temps de calcul raisonnables, nous allons considerer une zone geographique dans laquelle
# nous allons effectuer les tirages des pseudos absences. # Pour cela, on se base sur la zone d'etude totale, de laquelle on retire 
# la zone des occurrences des poissons. Techniquement, on definit la zone de tirage des pseudos abscences en se basant sur les bordures 
# de la zone d'etude totale, dans laquelle on integre les limites entre la zone des occurrences et la zone des pseudos abscences, en retirant 
# la zone des occurrences. 
# Ainsi, la zone d'etude totale est separee en 2: la zone des occurrences et la zone des tirages.



#####################################################################
### Definition des bornes de la zone des tirages des pseudos absences
#####################################################################


# On isole les occurrences de la 3eme decennie qui ne sont pas dans la meme zone que les occurrences des 2 premieres decennies
# (servira pour redefinir les bornes de la zone des occurrences)
area_d3 <- OCCd3[which(OCCd3$longitude > 152 & OCCd3$latitude > -22),1:2]


# limite sud de la zone de tirage des pseudos absences
lim_south_d3 <- c(area_d3[which.max(area_d3$longitude),1], area_d3[which.max(area_d3$latitude),2])
lim_south_d3[1] <- lim_south_d3[1] + lim
lim_south_d3[2] <- lim_south_d3[2] + lim

# limite extreme sud
bound_south_d3 <- c(lim_south_d3[1], min(coord_bnd_d3$latitude))

# limite du milieu de zone
lim_mid_d3 <- c(x=152.6686 + lim, y=-21.46962 + lim)

# limite nord
lim_north_d3 <- as.numeric(OCCd1[which.max(OCCd1$latitude),1:2])

# limite extreme nord
bound_north_d3 <- c(min(coord_bnd_d3$longitude), lim_north_d3[2])


# Initialisation de la zone de tirage des pseudo absences :
# On se base sur les bordures de la zone d'etude totale, desquelles on retire les coordonnees 
# communes entre la zone des occurrences et la zone de tirage des pseudos abscences pour les remplacer.

coord_bnd_d3 <- coord_bnd[-which(coord_bnd[,1] < bound_south_d3[1] & coord_bnd[,2] < lim_north_d3[2]),]

###################################################################################################
### Definition des nouvelles bordures (sud et diagonale) de la zone de tirage des pseudos absences
###################################################################################################


### Definition de la bordure sud ###

# Initialisation des latitudes des coordonnees de la bordure sud
i <- bound_south_d3[2]
j <- bound_south_d3[1]
# ajout d'une bordure verticale (longitude fixe) entre la latitude de la limite extreme sud et celle de la limite sud
while(i < lim_south_d3[2]){
  i <- i + 0.01
  coord_bnd_d3 <- rbind(coord_bnd_d3, c(j,i))
}
#plot(coord_bnd_d3, xlim=c(140,160), ylim=c(-25,-10), col='red')



### Definition de la bordure diagonale ###

# on definit le pas de progression des longitudes en fonction du pas des latitudes et de l'ensemble de definition des latitudes
# entre la limite sud et la limite nord
S2_d3 <- seq(lim_south_d3[2], lim_north_d3[2], by = 0.01)
pas2 <- (lim_south_d3[1] - lim_north_d3[1])/length(S2_d3)
# Initialisation des latitudes et des longitudes des coordonnees de la bordure diagonale
i <- lim_north_d3[1]
j <- lim_north_d3[2]
#ajout de la bordure diagonale entre la limite sud et la limite nord
while(i < lim_south_d3[1]){
  i <- i + pas2
  j <- j - 0.01
  coord_bnd_d3 <- rbind(coord_bnd_d3, c(i,j))
}
#plot(coord_bnd_d3, xlim=c(140,160), ylim=c(-25,-10), col='green')

#definition de la zone de tirage des pseudos absences de la 3eme decennie
Polys_d3 <- Polygon(coords = as.matrix(coord_bnd_d3, nrow = nrow(coord_bnd_d3), ncol = 2))


##############################################################
### Tirage aleatoire des pseudos absences pour chaque decennie
##############################################################


# Tirage aleatoire des pseudos absences de la 1ere decennie :
# il se trouve que parfois, certains points tires peuvent etre dans la zone des occurrences (et non plus la zone des pseudos absences).
# On decide alors que tant que le point tire est dans la zone des poissons, on refait le tirage
# les coordonnees des pseudos absences sont enregistrees
for(i in 1:nrow(OCCd1)){
  abs <- sample(spsample(Polys_d3,n=1,type="random", precision = 4))
  while(point.in.polygon(coordinates(abs)[1], coordinates(abs)[2], c(bound_south_d3[1], lim_south_d3[1], lim_north_d3[1], bound_north_d3[1], bound_north_d3[1]), c(bound_south_d3[2], lim_south_d3[2], lim_north_d3[2], bound_north_d3[2], bound_south_d3[2])) == 1){
    abs <- sample(spsample(Polys_d3,n=1,type="random", precision = 4))
  }
  absd1[i,] <- coordinates(abs)[1,]
}

plot(coord_bnd_d3, xlim=c(140,160), ylim=c(-25,-10), col='green')
par(new=T)
plot(absd1, xlim=c(140,160), ylim=c(-25,-10), col='red')
# toutes les pseudo absences de la 1ere decennie sont dans la zone de tirage



# Tirage aleatoire des pseudos absences de la 2eme decennie :
# tant que le point tire est dans la zone des poissons, on refait le tirage.
# Les coordonnees des pseudos absences sont enregistrees.
for(i in 1:nrow(OCCd2)){
  abs <- sample(spsample(Polys_d3,n=1,type="random", precision = 4))
  while(point.in.polygon(coordinates(abs)[1], coordinates(abs)[2], c(bound_south_d3[1], lim_south_d3[1], lim_north_d3[1], bound_north_d3[1], bound_north_d3[1]), c(bound_south_d3[2], lim_south_d3[2], lim_north_d3[2], bound_north_d3[2], bound_south_d3[2])) == 1){
    abs <- sample(spsample(Polys_d3,n=1,type="random", precision = 4))
  }
  absd2[i,] <- coordinates(abs)[1,]
}

plot(coord_bnd_d3, xlim=c(140,160), ylim=c(-25,-10), col='green')
par(new=T)
plot(absd1, xlim=c(140,160), ylim=c(-25,-10), col='red')
# Toutes les pseudo absences de la 2eme decennie sont dans la zone de tirage.


# Tirage aleatoire des pseudos absences de la 3eme decennie :
# tant que le point tire est dans la zone des poissons, on refait le tirage.
# Les coordonnees des pseudos absences sont enregistrees.
for(i in 1:nrow(OCCd3)){
  abs <- sample(spsample(Polys_d3,n=1,type="random", precision = 4))
  while(point.in.polygon(coordinates(abs)[1], coordinates(abs)[2], c(bound_south_d3[1], lim_south_d3[1], lim_north_d3[1], bound_north_d3[1], bound_north_d3[1]), c(bound_south_d3[2], lim_south_d3[2], lim_north_d3[2], bound_north_d3[2], bound_south_d3[2])) == 1){
    abs <- sample(spsample(Polys_d3,n=1,type="random", precision = 4))
  }
  absd3[i,] <- coordinates(abs)[1,]
}

plot(coord_bnd_d3, xlim=c(140,160), ylim=c(-25,-10), col='green')
par(new=T)
plot(absd3, xlim=c(140,160), ylim=c(-25,-10), col='red')
# Toutes les pseudos absences de la 3eme decennie sont dans la zone de tirage.


#################################################################
### Compilation des pseudos absences generes en un seul document
#################################################################


# Rassemblement des coordonnees des pseuods absences des 3 decennies

ABS <- rbind(absd1,absd2,absd3)
ABS <- data.frame(ABS)
colnames(ABS) <- c('longitude', 'latitude')

# Sauvegarde des pseudos absences sous un CSV
write.csv(ABS,"ABS.csv", row.names = TRUE)

# Ajout d'ue colonne presence remplie de 0 ou 1, correspondant respectivement aux pseudos absences et aux presences (occurences)
OCC <- OCC[,-3]
OCC$presence <- NA
OCC$presence <- rep(1, nrow(OCC))
ABS$decade <- NA
ABS$decade <- OCC$decade
ABS$presence <- NA
ABS$presence <- rep(0, nrow(ABS))

# Fusion des absences et des occurrences pour ensuite analyser facilement les donnees
DATA <- rbind(OCC, ABS)

# ainsi PA$resp   qu'on a dans le script de l'article sera  DATA$presence   


############# Ajout des variables pour nos donnees occurences et absences a  partir des coordonnees  ##############

# For all the fishes and pseudo absence, we attribute the closest observation (thanks to coordinates) of all of our variables : geomorphology, temperature, salinity, depth

# Importing geomorphic features
# Ce csv a Ã©tÃ© crÃ©Ã© Ã  partir des donnÃ©es sur https://www.deepreef.org/bathymetry/65-3dgbr-bathy.html 
# pour chaque fichier shapefile les commandes suivantes ont Ã©tÃ© effectuÃ©es, puis tout a Ã©tÃ© fusionnÃ© : 
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

# faire reprÃ©sentation graphique ? 
# Exemple: on trace les coordonnÃƒÂ©es des points du canyon
#plot(coordinates(my_spdf_canyon),pch=20)
# On ajoute le plateau
#points(coordinates(my_spdf_plateau),pch=17,col="orange")
# Faire un plot reprÃ©sentant tout le geomorphic ??
# plot(coordinate(geomorphic))

for (k in 1:nrow(DATA)){
  d = sqrt((DATA$longitude[k]-geomorphic$lon)^2 + (DATA$latitude[k]-geomorphic$lat)^2) 
  i_dmin = which.min(d)
  DATA$geomorphic[k] = geomorphic$type[i_dmin]
}

# Importing climatic data for each time period (tempÃ©rature et salinitÃ©)
# Ici on a donnÃ©es oÃ¹ les tempÃ©ratures sont triÃ©es en 3 catÃ©gories en fonction de la profondeur dans la mer

download.file("http://sdm.dev-lab.net/Temperature_1985_1994.RData",
              destfile = "Temperature_1985_1994.RData", mode = "wb")
download.file("http://sdm.dev-lab.net/Salinity_1985_1994.RData",
              destfile = "Salinity_1985_1994.RData", mode = "wb")
Temperature1 <- brick(get(load("Temperature_1985_1994.RData")))
Salinity1 <- brick(get(load("Salinity_1985_1994.RData")))
spplot(Salinity1,names.attr=c("Bottom","0-50m","0-200m"),main="Salinity 1985_1994")
spplot(Temperature1, names.attr=c("Bottom","0-50m","0-200m"), main="Temperature 1985_1994")

# c'est les large spatialpixel data frame Temperature et Salinity crÃ©e en mÃªme temps qui nous interessent. 
# le problÃ¨me c'est qu'Ã  chaque fois qu'on charge un nouveau raster tempÃ©rature (ou salinitÃ©) celui lÃ  va Ãªtre changer
# Ainsi on rÃ©cupÃ¨re les donnÃ©es de temperatures et salinitÃ© qui nous interessent pour ce decade (pour data frame OCC et ABS), puis on charge un nouveau jeu de donnÃ©es tempÃ©ratures (en salinitÃ©)
# les coordonnÃ©es des data frame tempÃ©rature et salinity sont les mÃªmes (mÃªme pas de donnÃ©es originelle NOAA). Ainsi on peut ne chercher la coord la plus proche qu'une fois
# The fish studied lives on the surface of the sea, thus, we will get temperature and salinity of surface water. 
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

# On rÃ©flÃ©chit Ã  ajouter des donnÃ©es sur les sÃ©diments venant de MARS  http://dbforms.ga.gov.au/pls/www/npm.mars.search
# mais il faut qu'on Ã©tudie plus cet aspect et qu'on dÃ©cide quelle forme de data on veut (rÃ©cupÃ©rer que des sÃ©diments Ã  la surface)


# Avant analyse il faudra certainement "neutraliser" les coordonnÃ©es pour ne pas qu'elles soient considÃ©rÃ©es comme une variable explicative
DATA$longitude <- 1:nrow(DATA)
DATA$latitude <- 1:nrow(DATA) 

# Le tableau est prÃªt Ã  Ãªtre analyser ! 
write.csv(DATA,"DATA.csv", row.names = TRUE)
