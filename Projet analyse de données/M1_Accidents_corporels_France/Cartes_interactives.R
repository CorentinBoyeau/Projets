library(sf)
library(tmap)
library(dplyr)

############## Importation données ###################

data <- read.csv("data/Accidents_clean.csv", header=T, sep=';', stringsAsFactors = F)
map <- st_read("data/shp/geoflar-departements-2015.shp", stringsAsFactors=F)
population <- read.csv("data/Population_departements.csv",header=T, sep=';',stringsAsFactors=F, fileEncoding="UTF-8-BOM")

############## Trie des données par département ###################

data_dep <- aggregate(subset(data, select=c(car_involve,bike_involve,hospitalise_nb,tue_nb)), by=list(data$dep), FUN= sum) 
colnames(data_dep) <- c("code_dept","nbr_accidents_voiture","nbr_accidents_velo","hospitalise_nb","tue_nb")


#On remplace les "1" par des "01", "2" par "02" ... pour avoir la même notation que dans le dataset map et population :
pos <- which(unique(data_dep$code_dept)%in%c("1","2","3","4","5","6","7","8","9"))
data_dep$code_dept <- replace(data_dep$code_dept, pos, c("01","02","03","04","05","06","07","08","09"))

############ Création de variables par 10 000 habitants ################

data_and_pop <- inner_join(population, data_dep)

data_and_pop$accidents_voiture_10000 = round(10000* data_and_pop$nbr_accidents_voiture / data_and_pop$population, digits=1)
data_and_pop$accidents_velo_10000 = round(10000* data_and_pop$nbr_accidents_velo / data_and_pop$population, digits=1)
data_and_pop$hospitalise_nb_10000 = round(10000* data_and_pop$hospitalise_nb / data_and_pop$population, digits=1)
data_and_pop$tue_nb_10000 = round(10000* data_and_pop$tue_nb / data_and_pop$population, digits=1)

############ Fond de carte + données ######################"

map_and_data <- inner_join(map, data_and_pop)

map_and_data <- map_and_data[!map_and_data$code_dept %in% c("971","972","973","974","976"),]  
#On enlève les départements d'outre mer pour plus de visibilité


############ Création d'une carte intéractive avec plusieurs couches en format html #####################



tm_shape(map_and_data, name="Accidents voiture/10000 hab (2010 à 2017)")+
  tm_polygons("accidents_voiture_10000", id="nom_dept",title = "Nombre d'accidents de voiture pour 10000 habitants (2010 à 2017)", popup.vars=c("Nombre d'accidents de voiture pour 10000 habitants"="accidents_voiture_10000"), palette="Greens")+
  tm_shape(map_and_data, name="Nombre d'hospitalisation pour 10000 habitants (2010 à 2017)")+
  tm_polygons("hospitalise_nb_10000", id="nom_dept",title = "Nombre d'hospitalisation pour 10000 habitants (2010 à 2017)", popup.vars=c("Nombre d'hospitalisation pour 10000 habitants"="hospitalise_nb_10000"), palette="Purples")+
  tm_shape(map_and_data, name="Nombre de mort pour 10000 habitants (2010 à 2017)")+
  tm_polygons("tue_nb_10000", id="nom_dept",title = "Nombre de mort pour 10000 habitants (2010 à 2017)", popup.vars=c("Nombre de mort pour 10000 habitants"="tue_nb_10000"), palette="Greys")

tmap_mode("view")
Accidents_corporels_France_2010_a_2017 <- tmap_last()

tmap_save(Accidents_corporels_France_2010_a_2017, "Cartes_interactives.html")