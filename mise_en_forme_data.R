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
#library(Rmpfr)

### Set working directory 
# setwd("C:/Users/flori/Desktop/cours/MLB")

# Download occurrences from OBIS  
# on garde seulement les colonnes coordonnÃ©es et date d'observation (pour pouvoir rÃ©cupÃ©rer les donnÃ©es environnementales correspondantes)

OCC <- robis::occurrence(scientificname = "Chaetodon rainfordi" ) # Download, be carefull of the firewall
if (dim(OCC)[1] > 0) {
  OCC <- cbind(OCC[, c("decimalLongitude", "decimalLatitude", "date_year")])
  names(OCC) <- c("longitude", "latitude", "year")
}


# enlever les lignes oÃ¹ l'on n'a pas de date d'observation, ou si le poisson a Ã©tÃ© observÃ© avant 1985 (car pas assez d'observations par decades)
OCC <- na.omit(OCC, cols='year')
OCC <- OCC[-which(OCC$year<1985),]
OCC <- OCC[-which(OCC$year>2012),] # car on n'a pas de tempÃ©ratures dispo aprÃ¨s 2012. MÃªme sur NOAA. 
# doit on vraiment enlever ces poissons ? car il y en a quand mÃªme environ 900 (sur environ 4900)


# associer pour chaque annÃ©e la decade correspondante pour pouvoir rÃ©cupÃ©rer tempÃ©rature et salinitÃ©
OCC$decade <- NA
OCC$decade <- ifelse(OCC$year >= 1985 & OCC$year <= 1994, 1, OCC$decade)
OCC$decade <- ifelse(OCC$year >= 1995 & OCC$year <= 2004, 2, OCC$decade)
OCC$decade <- ifelse(OCC$year >= 2004 & OCC$year <= 2012, 3, OCC$decade)

# Importing boundries coordinates 
# This limit corresponds to the area studied, for which we have information on its geomorphology

boundaries <- readOGR( 
  dsn= "3dgbr_geomorph/shape", 
  layer = "qld_gbrwha_cscz",
  verbose=FALSE)

# We plot fishes to see if some of them are out of boundaries
plot(OCC$longitude, OCC$latitude, xlim=c(120,155), ylim=c(-28,-14))
par(new=T)
plot(coordinates(boundaries)[[1]][[1]],pch=20, xlim=c(120,155), ylim=c(-28,-14))

#there are some fishes out of boundaries, we delete them
OCC <- OCC[-which(OCC$longitude<140),]
OCC <- OCC[-which(OCC$latitude< -25),]

plot(OCC$longitude, OCC$latitude, xlim=c(120,155), ylim=c(-28,-14))
par(new=T)
plot(coordinates(boundaries)[[1]][[1]],pch=20, xlim=c(120,155), ylim=c(-28,-14))

#Now all the fishes are in our boundaries
#The Occurrence data are ready to be used



##### on va maintenant crÃ©er la base de notre dataframe absences
# on ajoutera les colonnes variables pour chaque data frame en parallÃ¨le ensuite
# mais pour crÃ©er les absences il faut rester dans notre zone d'Ã©tude. Nous allons nous servir de boundaries

# initialisation des coordonnees de la zone d'etude des 2 premieres decennies
coord_bnd <- coordinates(boundaries)[[1]][[1]]
colnames(coord_bnd) <- c('longitude','latitude')
coord_bnd <-as.data.frame(coord_bnd)


###exclusion zone interdite par decennie
OCCd1 <- OCC[which(OCC$decade == 1),] # on separe chaque decennie
OCCd2 <- OCC[which(OCC$decade == 2),]
OCCd3 <- OCC[which(OCC$decade == 3),]

# initialisation des matrices contenants les pseudos absences par decennie
absd1 <- matrix(as.numeric(NA), nrow = nrow(OCCd1), ncol = 2)
absd2 <- matrix(as.numeric(NA), nrow = nrow(OCCd2), ncol = 2)
absd3 <- matrix(as.numeric(NA), nrow = nrow(OCCd3), ncol = 2)

# Si des pseudos absences generees se trouvent dans une zone a proximite d'une occurrence, pour limiter le temps de calcul,
# on refait un tirage aleatoire dans une aire specifique eloignee de toutes les occurrences, incluse dans notre zone d'etude

# on etudie les occurrences des poissons selon leurs coordonnees, puis on decide de definir la zone
# ou il n'y aura que des presences (aucune pseudo absence)
plot(OCCd1$longitude, OCCd1$latitude, xlim=c(140,160), ylim=c(-25,-10), col='green')

# definition des bordures de la zone sans pseudos absences
# Ces limites sont définies pour les deux premières décennies (POURQUOI ?)



# texte pour expliquer lim ! le mettre à endroit approprié 

# les coordonnees sont des degres då¶°imaux. 
# une variation ï¿½ partir du 5eme chiffre apres la virgule correspond a une variation d'environ 1.11 m a l'equateur.
# dans un soucis de simplification, et de part la petitesse de la superficie des zones calculees autour des occurrences,
# nous allons negliger la courbure de la Terre et considerer une variation de 1.11 m dans la Grande Barriere de corail.

# Rayon de la Terre : 6400 km
# La Grande Barrié‹¨e de corail se situe environ a 20 degres Sud
# Rayon de la Terre dans le plan passant par la latitude 20 degres Sud et parallele au plan de l'equateur : 6400 * cos(20) = 6014 km
# Le rapport entre la variation en metres entre la latitude a l'equateur et a la latitude de laGrande Barriere de corail est : 6014/6400 = 0.93969
# La variation au niveau de la Grande Barriere de corail est donc de 1.11 * 0.93969 = 1.04

lim = 1000/(100000 * 1.04) #on då¶¨init les limites des zones oï¿½ l'on ne fera pas les tirages des pseudos absences avec un rayon de 1 km autour de l'occurrence, que l'on convertit en variation de degres decimaux. 
#cette zone represente l'aire ou les poissons n'ont pas etes observes mais sont consideres comme existants





# limite sud
lim_south <- OCCd1[which.max(OCCd1$longitude),1:2]
lim_south[1] <- lim_south[1] + lim
lim_south <- as.numeric(lim_south)

# limite extreme sud
bound_south <-c(lim_south[1], min(coord_bnd$latitude))

# limite du milieu de zone
lim_mid <- c(x=152.5743 + lim, y=-21.46962 + lim)

# limite nord
lim_north <- as.numeric(OCCd1[which.max(OCCd1$latitude),1:2])
lim_north[1] <- lim_north[1] + lim
lim_north[2] <- lim_north[2] + lim

# limite extreme nord
bound_north <- c(min(coord_bnd$longitude), lim_north[2])

#on supprime les points de bordure ou se situe la zone des occurrences pour la redefinir selon le critere de distance (lim de 1 km de rayon)
coord_bnd <- coord_bnd[-which(coord_bnd$longitude < bound_south[1] & coord_bnd$latitude < lim_north[2]),]

#initialisation des limites de la zone de tirage pour la 3eme decennie, ou il y aura un changement par rapport aux 2 premieres
coord_bnd_d3 <- coord_bnd 

#plot(coord_bnd, xlim=c(140,160), ylim=c(-25,-10), col='green')

#definition de la bordure verticale au sud de la zone
for (i in seq(bound_south[2], lim_south[2], by = 0.01)){
  coord_bnd <- rbind(coord_bnd, c(lim_south[1],i))
}
plot(coord_bnd, xlim=c(140,160), ylim=c(-25,-10), col='green')

# on definit le pas de progression des longitudes en fonction du pas et de l'ensemble de definition des latitudes
S <- seq(lim_south[2], lim_mid[2], by = 0.01)
pas <- (lim_mid[2] - lim_south[2])/length(S)
# ajout de la bordure entre le sud et le milieu de zone
i <- lim_mid[1]
j <- lim_mid[2]
while(i < lim_south[1]){
  i <- i + 0.01
  j <- j - pas
  coord_bnd <- rbind(coord_bnd, c(i,j))
}
plot(coord_bnd, xlim=c(140,160), ylim=c(-25,-10), col='green')

# on definit le pas de progression des longitudes en fonction du pas et de l'ensemble de definition des latitudes
S2 <- seq(lim_north[1], lim_mid[1], by = 0.01)
pas2 <- (lim_mid[1] - lim_north[1])/length(S2)

# ajout de la bordure au milieu de la zone d'etude
i <- lim_north[1]
j <- lim_north[2]
while(i < lim_mid[1]){
  i <- i + 0.01
  j <- j - pas2
  coord_bnd <- rbind(coord_bnd, c(i,j))
}
plot(coord_bnd, xlim=c(140,160), ylim=c(-25,-10), col='green')


# polygone de la zone où il ne doit pas y avoir de pseudos absences pour la 1ere decennie
poly_OCC <- Polygon(coords = matrix(c(bound_south, lim_mid, lim_north, bound_north, bound_south), nrow = 5, ncol = 2, byrow = T))

# polygone où se feront les tirages des pseudos absences dd la 1ere decennie
Polys <- Polygon(coords = as.matrix(coord_bnd, nrow = nrow(coord_bnd), ncol = 2))


# tirage aleatoire des pseudos absences , tant qu'elles se trouvent dans la zone des presences, on refait le tirage
# puis on enregistre les coordonnees des pseudos abscences
for(i in 1:nrow(OCCd1)){
  abs <- sample(spsample(Polys,n=1,type="random", precision = 4))
  while(point.in.polygon(coordinates(abs)[1], coordinates(abs)[2], c(bound_south[1], lim_mid[1], lim_north[1], bound_north[1]), c(bound_south[2], lim_mid[2], lim_north[2], bound_north[2])) == 1){
    abs <- sample(spsample(Polys,n=1,type="random", precision = 4))
  }
  absd1[i,] <- coordinates(abs)[1,]
}


# tirage aleatoire des pseudos absences , tant qu'elles se trouvent dans la zone des presences, on refait le tirage
#puis on enregistre les coordonnees des pseudos abscences
for(i in 1:nrow(OCCd2)){
  abs <- sample(spsample(Polys,n=1,type="random", precision = 4))
  while(point.in.polygon(coordinates(abs)[1], coordinates(abs)[2], c(bound_south[1], lim_mid[1], lim_north[1], bound_north[1]), c(bound_south[2], lim_mid[2], lim_north[2], bound_north[2])) == 1){
    abs <- sample(spsample(Polys,n=1,type="random", precision = 4))
  }
  absd2[i,] <- coordinates(abs)[1,]
}


# on isole les occurrences qui ne sont pas dans la meme zone que les 2 premieres decennies
area_d3 <- OCCd3[which(OCCd3$longitude > 152 & OCCd3$latitude > -22),1:2]

### on redefinit les bordures de la zone de presence des poissons de la 3eme decennie

# limite sud
lim_south_d3 <- c(area_d3[which.max(area_d3$longitude),1], area_d3[which.max(area_d3$latitude),2])
lim_south_d3[1] <- lim_south_d3[1] + lim
lim_south_d3[2] <- lim_south_d3[2] + lim

# limite extreme sud
bound_south_d3 <- bound_south
bound_south_d3[1] <- lim_south_d3[1]

# limite du milieu de zone
lim_mid_d3 <- lim_mid
lim_mid_d3[1] <- lim_south_d3[1]

# limite nord
lim_north_d3 <- lim_north

# limite extreme nord
bound_north_d3 <- bound_north

# on definit le pas de progression des longitudes en fonction du pas et de l'ensemble de definition des latitudes
S1_d3 <- seq(bound_south_d3[2], lim_south_d3[2], by = 0.01)
pas <- (lim_south_d3[2] - bound_south_d3[2])/length(S1_d3)
#ajout de la bordure verticale au sud de la zone
i <- bound_south_d3[2]
j <- bound_south_d3[1]
while(i < lim_south_d3[2]){
  i <- i + 0.01
  coord_bnd_d3 <- rbind(coord_bnd_d3, c(j,i))
}
#plot(coord_bnd_d3, xlim=c(140,160), ylim=c(-25,-10), col='green')

# on definit le pas de progression des longitudes en fonction du pas et de l'ensemble de definition des latitudes
S2_d3 <- seq(lim_south_d3[2], lim_north_d3[2], by = 0.01)
pas2 <- (lim_south_d3[1] - lim_north_d3[1])/length(S2_d3)
#ajout de la bordure diagonale au centre de la zone
i <- lim_north_d3[1]
j <- lim_north_d3[2]
while(i < lim_south_d3[1]){
  i <- i + pas2
  j <- j - 0.01
  coord_bnd_d3 <- rbind(coord_bnd_d3, c(i,j))
}
#plot(coord_bnd_d3, xlim=c(140,160), ylim=c(-25,-10), col='green')

#definition de la zone d'etude de la 3eme decennie
Polys_d3 <- Polygon(coords = as.matrix(coord_bnd_d3, nrow = nrow(coord_bnd_d3), ncol = 2))
#definition de la zone ou les pseudos absences ne doivent pas se trouver
ban_d3 <- rbind(bound_south_d3, lim_south_d3, lim_north_d3, bound_north_d3)
poly_d3 <- Polygon(coords = ban_d3)

# on tire les pseudos absenses aleatoirement dans la zone d'etude.
# tant que le point tire est dans la zone des poissons, on refait le tirage
# les coordonnees des pseudos absences sont enregistrees
for(i in 1:nrow(OCCd3)){
  abs <- sample(spsample(Polys_d3,n=1,type="random", precision = 4))
  while(point.in.polygon(coordinates(abs)[1], coordinates(abs)[2], c(bound_south_d3[1], lim_south_d3[1], lim_north_d3[1], bound_north_d3[1]), c(bound_south_d3[2], lim_south_d3[2], lim_north_d3[2], bound_north_d3[2])) == 1){
    abs <- sample(spsample(Polys,n=1,type="random", precision = 4))
  }
  absd3[i,] <- coordinates(abs)[1,]
}





#on rassemble les pseudos absences
ABS <- rbind(absd1,absd2,absd3)
ABS <- data.frame(ABS)
colnames(ABS) <- c('longitude', 'latitude')
write.csv(ABS,"ABS.csv", row.names = TRUE)

# on ajoute une colonne presence remplie de 0 ou 1 pour pouvoir ensuite lors de notre analyse assimiler que 1=prÃ©sence et 0=absence
OCC <- OCC[,-3]
OCC$presence <- NA
OCC$presence <- rep(1, nrow(OCC))
ABS$decade <- NA
ABS$decade <- OCC$decade
ABS$presence <- NA
ABS$presence <- rep(0, nrow(ABS))

# fusionner les deux tableaux OCC et ABS pour ensuite analyser facilement les donnÃ©es
DATA <- rbind(OCC, ABS)
# ainsi PA$resp   qu'on a dans le script de l'article sera  DATA$presence   


############# Ajout des variables pour nos donnÃ©es occurences et absences Ã  partir des coordonnÃ©es  ##############

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
