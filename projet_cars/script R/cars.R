#Chargement de l'environemment de travail
#getwd()
setwd("./base_langage_R/prepared_data")

#chargement jeu de donnée
cars_set <- read.table(
  file = "Cars.txt",
  header = TRUE,
  sep = "\t",
  quote = "\""
)

#Vérification du chargement des données
head(cars_set)

#charger la bibliothèque dplyr
library(dplyr)

#Selection d'un sous ensemble de colonnes
temp <- select(
  .data = cars_set,
  Transmission,
  Cylinders,
  Fuel.Economy
)

#Vérification du chargement des données
head(temp)

#Filtrer un sous ensemble de ligne
temp2 <- filter(
  .data = cars_set,
  Transmission == "Automatic"
)

#Vérification du chargement des données
head(temp2)
