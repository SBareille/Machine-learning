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

# enlever les lignes où l'on n'a pas de date d'observation, ou si le poisson a été observé avant 1985
OCC <- na.omit(OCC, cols='year')
OCC <- OCC[-which(OCC$year<1985),]

# enlever les poissons qui sont en dehors de notre environnement d'étude et ceux qui ont longitudes et latitudes =0
# pas possible encore car on a besoin de manipuler ces jeux de données avant.
# zero.coord <- which(OCC$longitude == 0 & OCC$latitude == 0)
# OCC <- OCC[ -which(OCC$longitude == 0 & OCC$latitude == 0), ]

############# Création du date frame complet occurence ##############

# à partir des longitudes et latitudes des occurences, récupérer les données environnementales correspondantes et les ajouter au tableau

# associer pour chaque année la decade correspondante pour pouvoir récupérer température et salinité
# il y a un problème -> le régler
OCC$decade <- NA
OCC$decade <- ifelse(OCC$year >= 1985 & OCC$year <= 1994, 1, OCC$decade)
OCC$decade <- ifelse(OCC$year >= 1995 & OCC$year <= 2004, 2, OCC$decade)
OCC$decade <- ifelse(OCC$year >= 2004 & OCC$year <= 2012, 3, OCC$decade)
if(length(which(is.na(OCC$decade))) > 0){OCC = OCC[-which(is.na(OCC$decade)), ]}
OCC$Temperature <- NA
OCC$Salinity <- NA
#blabla


#Importing climatic data for each time period
# voir si on n'importe pas le jeu de données directement de NOAH.
# Ici on a données où les températures sont triées en 3 catégories en fonction de la profondeur dans la mer. Est-ce utile ici ? faire nous même les profondeurs ? voir comment c'est présenté sur NOAH
# se servir des profondeur d'observation des poissons (qu'on a jarté de notre jeu d'occurance) pour savoir exactement la tempé à ce point d'obs

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

# # Importing boudries coordinates 
# c'était pour piocher les pseudo absences mais finalement on n'a pas l'air d'en avoir besoin

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
# # ajouter la limite nord (à latitude = ). A faire ou non ? ça dépend un peu de comment on décide de faire notre tirage d'absence. 
# 
# plot(boundaries,pch=20)


# Importing geomorphic features
# a t'on besoin de réécrire le chemin de téléchargement internet ? 

my_spdf_cay <- readOGR( 
  dsn= "3dgbr_geomorph/shape", 
  layer = "coralsea_cay",
  verbose=FALSE)
cc = coordinates(my_spdf_cay)
geomorphic_1 = data.frame(cc)
colnames(geomorphic_1) = c("lon","lat")
geomorphic_1$type = "cay"

my_spdf_dryreef <- readOGR( 
  dsn= "3dgbr_geomorph/shape", 
  layer = "coralsea_dryreef",
  verbose=FALSE)
cc = coordinates(my_spdf_dryreef)
geomorphic_2 = data.frame(cc)
colnames(geomorphic_2) = c("lon","lat")
geomorphic_2$type = "dryreef"

my_spdf_reef <- readOGR( 
  dsn= "3dgbr_geomorph/shape", 
  layer = "coralsea_reef",
  verbose=FALSE)
cc = coordinates(my_spdf_reef)
geomorphic_3 = data.frame(cc)
colnames(geomorphic_3) = c("lon","lat")
geomorphic_3$type = "reef"

my_spdf_ridge <- readOGR( 
  dsn= "3dgbr_geomorph/shape", 
  layer = "coralsea_ridge",
  verbose=FALSE)
cc = coordinates(my_spdf_ridge)
geomorphic_4 = data.frame(cc)
colnames(geomorphic_4) = c("lon","lat")
geomorphic_4$type = "ridge"

my_spdf_bank <- readOGR( 
  dsn= "3dgbr_geomorph/shape", 
  layer = "coralsea_bank",
  verbose=FALSE)
cc = coordinates(my_spdf_bank)
geomorphic_5 = data.frame(cc)
colnames(geomorphic_5) = c("lon","lat")
geomorphic_5$type = "bank"

my_spdf_knoll <- readOGR( 
  dsn= "3dgbr_geomorph/shape", 
  layer = "coralsea_knoll",
  verbose=FALSE)
cc = coordinates(my_spdf_knoll)
geomorphic_6 = data.frame(cc)
colnames(geomorphic_6) = c("lon","lat")
geomorphic_6$type = "knoll"

my_spdf_canyon <- readOGR( 
  dsn= "3dgbr_geomorph/shape", 
  layer = "coralsea_canyon",
  verbose=FALSE)
cc = coordinates(my_spdf_canyon)
geomorphic_7 = data.frame(cc)
colnames(geomorphic_7) = c("lon","lat")
geomorphic_7$type = "canyon"

my_spdf_seamount <- readOGR( 
  dsn= "3dgbr_geomorph/shape", 
  layer = "coralsea_seamount",
  verbose=FALSE)
cc = coordinates(my_spdf_seamount)
geomorphic_8 = data.frame(cc)
colnames(geomorphic_8) = c("lon","lat")
geomorphic_8$type = "seamount"

my_spdf_shelf <- readOGR( 
  dsn= "3dgbr_geomorph/shape", 
  layer = "gbr_shelf",
  verbose=FALSE)
cc = coordinates(my_spdf_shelf)
geomorphic_9 = data.frame(cc)
colnames(geomorphic_9) = c("lon","lat")
geomorphic_9$type = "shelf"

my_spdf_slope <- readOGR( 
  dsn= "3dgbr_geomorph/shape", 
  layer = "coralsea_slope",
  verbose=FALSE)
cc = coordinates(my_spdf_slope)
geomorphic_10 = data.frame(cc)
colnames(geomorphic_10) = c("lon","lat")
geomorphic_10$type = "slope"

my_spdf_terrace <- readOGR( 
  dsn= "3dgbr_geomorph/shape", 
  layer = "coralsea_terrace",
  verbose=FALSE)
cc = coordinates(my_spdf_terrace)
geomorphic_11 = data.frame(cc)
colnames(geomorphic_11) = c("lon","lat")
geomorphic_11$type = "terrace"

my_spdf_plateau <- readOGR( 
  dsn= "3dgbr_geomorph/shape", 
  layer = "coralsea_plateau",
  verbose=FALSE)
cc = coordinates(my_spdf_plateau)
geomorphic_12 = data.frame(cc)
colnames(geomorphic_12) = c("lon","lat")
geomorphic_12$type = "plateau"

my_spdf_valley <- readOGR( 
  dsn= "3dgbr_geomorph/shape", 
  layer = "coralsea_valley",
  verbose=FALSE)
cc = coordinates(my_spdf_valley)
geomorphic_13 = data.frame(cc)
colnames(geomorphic_13) = c("lon","lat")
geomorphic_13$type = "valley"

my_spdf_trough <- readOGR( 
  dsn= "3dgbr_geomorph/shape", 
  layer = "coralsea_trough",
  verbose=FALSE)
cc = coordinates(my_spdf_trough)
geomorphic_14 = data.frame(cc)
colnames(geomorphic_14) = c("lon","lat")
geomorphic_14$type = "trough"

my_spdf_rise <- readOGR( 
  dsn= "3dgbr_geomorph/shape", 
  layer = "coralsea_rise",
  verbose=FALSE)
cc = coordinates(my_spdf_rise)
geomorphic_15 = data.frame(cc)
colnames(geomorphic_15) = c("lon","lat")
geomorphic_15$type = "rise"

my_spdf_basin <- readOGR( 
  dsn= "3dgbr_geomorph/shape", 
  layer = "coralsea_basin",
  verbose=FALSE)
cc = coordinates(my_spdf_basin)
geomorphic_16 = data.frame(cc)
colnames(geomorphic_16) = c("lon","lat")
geomorphic_16$type = "basin"

my_spdf_abyssalplain <- readOGR( 
  dsn= "3dgbr_geomorph/shape", 
  layer = "coralsea_abyssalplain",
  verbose=FALSE)
cc = coordinates(my_spdf_abyssalplain)
geomorphic_17 = data.frame(cc)
colnames(geomorphic_17) = c("lon","lat")
geomorphic_17$type = "abyssalplain"

# Création d'un fichier réportoriant toutes les informations géomorphiques
geomorphic = rbind(geomorphic_1,geomorphic_2, geomorphic_3, geomorphic_4, geomorphic_5, geomorphic_6,
                   geomorphic_7, geomorphic_8, geomorphic_9, geomorphic_10, geomorphic_11, geomorphic_12,
                   geomorphic_13, geomorphic_14, geomorphic_15, geomorphic_16, geomorphic_17)

# Exemple: on trace les coordonnÃ©es des points du canyon
#plot(coordinates(my_spdf_canyon),pch=20)
# On ajoute le plateau
#points(coordinates(my_spdf_plateau),pch=17,col="orange")
# Faire un plot représentant tout le geomorphic ??
# plot(coordinate(geomorphic))

# SI on a un point de prÃ©sence (par ex lon = 144, lat = -11)
lon_pr = 144
lat_pr = -11
d = sqrt((lon_pr-type_fond$lon)^2 + (lat_pr-type_fond$lat)^2) 
i_dmin = which.min(d)
lon_plus_proche = type_fond$lon[i_dmin]
lat_plus_proche = type_fond$lat[i_dmin]
fond_plus_proche = type_fond$type[i_dmin]


d_temp = sqrt((lon_pr-Temperature@coords[,1])^2+ (lat_pr-Temperature@coords[,2])^2) 
i_dtemp_min = which.min(d_temp)
temp_fond_plus_proche = Temperature@Bottom_mean[i_dtemp_min]
temp_vertical_plus_proche = Temperature@Vertical_mean[i_dtemp_min]
temp_surf_plus_proche = Temperature@Surface_mean[i_dtemp_min]






############# Création du date frame complet absence ##############

# definir autour de nos coordonnées d'occurence d'observation une zone dans laquelle il ne sera pas possible de piocher les absences
# a faire

# les coordonnees sont des degres d�cimaux. 
# une variation � partir du 5eme chiffre apres la virgule correspond a une variation d'environ 1.11 m a l'equateur.
# dans un soucis de simplification, et de part la petitesse de la superficie des zones calculees autour des occurrences,
# nous allons negliger la courbure de la Terre et considerer une variation de 1.11 m dans la Grande Barriere de corail.

# Rayon de la Terre : 6400 km
# La Grande Barri�re de corail se situe environ a 20 degres Sud
# Rayon de la Terre dans le plan passant par la latitude 20 degres Sud et parallele au plan de l'equateur : 6400 * cos(20) = 6014 km
# Le rapport entre la variation en metres entre la latitude a l'equateur et a la latitude de laGrande Barriere de corail est : 6014/6400 = 0.93969
# La variation au niveau de la Grande Barriere de corail est donc de 1.11 * 0.93969 = 1.04

lim = 2000/(100000 * 1.04) #on d�finit les limites des zones o� l'on ne fera pas les tirages des pseudos absences avec un rayon de 1000 m autour de l'occurrence, que l'on convertit en variation de degres decimaux. 
#cette zone represente l'aire ou les poissons n'ont pas etes observes mais sont consideres comme existants

OCC$occurrence <- NA
OCC$occurrence <- rep(1, nrow(OCC))

OCCd1 <- OCC[which(OCC$decade == 1),] # on separe chaque decennie
OCCd2 <- OCC[which(OCC$decade == 2),]
OCCd3 <- OCC[which(OCC$decade == 3),]

T1<-Sys.time()

for (i in 1:nrow(OCC)){# pour chaque occurrence
  for(j in 1:nrow(geomorphic)){
    if(sqrt((OCC[i,1] - geomorphic[j,1])^2 + (OCC[i,2] - geomorphic[j,2])^2) <= lim){# si les coordonnees utilisees plus tard pour generer les pseudo absences sont dans la zone, alors la donnee correspondante est supprimee
      geomorphic <- geomorphic[ -j,]
    }
  }
}
T2<-Sys.time()
Tdiff= difftime(T2, T1) 

par(mfrow = c(3,1))
# plot se^pare pour chaque decennie (uniquement visualisation, soit pimper soit tej)

plot(x=geomorphic[,1], y=geomorphic[,2], xlim=c(144,160), ylim=c(-25,-10), col='red')
par(new=T)
plot(x=OCCd1[,1], y=OCCd1[,2], xlim=c(144,160), ylim=c(-25,-10), col='green')


plot(x=geomorphic[,1], y=geomorphic[,2], xlim=c(144,160), ylim=c(-25,-10), col='red')
par(new=T)
plot(x=OCCd2[,1], y=OCCd2[,2], xlim=c(144,160), ylim=c(-25,-10), col='green')


plot(x=geomorphic[,1], y=geomorphic[,2], xlim=c(144,160), ylim=c(-25,-10), col='red')
par(new=T)
plot(x=OCCd3[,1], y=OCCd3[,2], xlim=c(144,160), ylim=c(-25,-10), col='green')



# piocher, au sein de notre zone d'étude (pour laquelle on a toutes les infos enviro), et en dehors de nos zones au voisinage d'une présence nouvellement délimitées des coordonnées qui correspondront aux endroits d'"absence"
# piocher autant d'absences que d'occurences

abs <- geomorphic #initialisation du dossier contenant les pseudo absences

for (i in 1:nrow(abs)){ # on vide les lignes (on veut un tableau vide a remplir au fur et a mesure)
  abs[i,] = NA
}
set.seed(1)
for (i in 1:nrow(OCC)){ # chaque ligne devient un tirage aleatoire parmi les donnees du sol de la Grande Barriere de corail
  abs[i,] = geomorphic[sample(1:nrow(geomorphic)),]
}
abs$espece <- NA
abs$espece <- rep(0, nrow(abs)) # on associe a ce tirage des absences

#faudra alors y ajouter les donnees des autres variables avant de la rbind avec OCC, puis utiliser OCC pour machine learning
# ajouter ensuite à partir de ce tableau ABS les colonnes avec les variables enviro correspondantes comme on l'a fait pour OCC
