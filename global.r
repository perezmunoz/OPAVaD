####################
###### global ######
####################

require(shiny)
require(leaflet)
require(ggplot2)
require(data.table)
require(bit64)
require(dplyr)
require(reshape2)

# Attributs de la data table index
varIndex <- c("siret", "rs", "naf", "ville", "lat", "lon", "ca")

# Attributs de la data table pour la carte
varMap <- c('siret', 'rs', 'date', 'naf', 'ville', 'lat', 'lon', 'ca', 'montant', 'transaction')

# Chargement de la data table répertoriant l'ensemble les informations sur tous les commerçants
index <- fread("C:/Users/CAvatar/Desktop/INDEX/index.txt", sep="\t")
setnames(x = index, old=names(index), new=varIndex)

# Structuration des données
index$siret <- as.character(index$siret)

# Attributs des data tables df.s et df.n
var <- c('montant','date','heure','siret','rs','naf','villecom','lon','lat','affilie','client','age','sexe','csp','optel','opmail','mail',
         'enfant','situation','anciennete','segment','score','avoir','appinternet','appmobile','villeclient','uu','carte','libelle','reseau',
         'paiement','retrait', 'niveau','typecompte','credit','debit','mensualite','restant','datepremire','datederniere','vide','ca')

# Variables utiles à l'initialisation de la carte dans la méthode comInBounds()
north <- 0.00342
east <- 0.0204407
south <- 0.00271
west <- 0.0203933

# Récupération du nom de la table SIRET du commerçant connecté
getNameSIRET <- function() {
  df.name <- paste('C:/Users/CAvatar/Desktop/SIRET/','NAF',KEY$naf,'/',KEY$siret,'.txt',sep = "")
  return(df.name)
}

# Récupération du nom de la table NAF du commerçant connecté
getNameNAF <- function() {
  df.name <- paste('C:/Users/CAvatar/Desktop/NAF/',KEY$naf,'.txt',sep = "")
  return(df.name)
}

###################################################################################################################
#                                     OPTIMISER LE CALCUL DU BARYCENTRE                                           #
###################################################################################################################

# Rayon de la Terre (km)
rayon = 6378.137

# Calcul la distance d'un commerçant au barycentre d'un client et renvoie TRUE ou FALSE s'il est dans son champ d'action
action <- function(dist) {
  #latC = 48.095356 * pi/180
  #longC = 1.637367 * pi/180
  latC = KEY$latitude * pi/180
  longC = -(KEY$longitude * pi/180)
  print(paste("latC", latC, "et longC", longC))
  #bar <- barycentre(recapitulatif(commercants))
  bar <<- df.barycentres
  for(i in 1:nrow(bar)) {
    print(paste("i =", i))
    separation <- acos(sin(latC)*sin(bar$Lat[i]*pi/180)+cos(latC)*cos(bar$Lat[i]*pi/180)*cos(longC+bar$Long[i]*pi/180)) * rayon
    if(separation <= dist) {
      bar$present[i] <<- as.logical("TRUE")
    } else {bar$present[i] <<- as.logical("FALSE")}
  }
  return(bar)
}

# Résume les données clients
recapitulatif <- function(df) {
  print("recap")
  if(nrow(df) == 0) {
    return()
  }
  # on regroupe les transactions par identifiant client, raison sociale du commerçant ainsi que sa géolocalisation
  resume <- df %>%
    group_by("client", "rs", "Lat", "Long") %>%
    summarise(n = n())
  return(resume)
}

# Calcul le barycentre pour chacun des clients
barycentre <- function(e) {
  # e est la liste des transactions pour UN client
  print("bary")
  clients <- unique(e[ , 1])
  print(clients)
  table <- matrix(0 ,nrow = length(clients), ncol = 3)
  print(table)
  df.barycentres <<- as.data.frame(table)
  names(df.barycentres) <<- c("client", "Lat", "Long")
  df.barycentres$client <<- as.character(df.barycentres$client)
  # On réalise autant d'itérations qu'il y a de clients
  for(k in 1:length(clients)) {
    print(paste("k =", k))
    df.barycentres[k, 1] <<- as.character(clients[k])
    grav <- subset(e, client == clients[k])
    # Initialisation des variables
    Gx <- 0
    Gy <- 0
    poids <- sum(grav$n)
    # Pour chaque client, on calcule les coordonnées de son point de gravité
    for(i in 1:nrow(grav)) {
      Gx <- Gx + (grav[i, 3] * grav[i, 5])
      Gy <- Gy + (grav[i, 4] * grav[i, 5])
    }
    # Gravité client stocké dans la data frame des barycentres
    df.barycentres[k, 2] <<- Gx / poids
    df.barycentres[k, 3] <<- Gy / poids
  }
  return(df.barycentres)
}

###################################################################################################################
#                                                                                                                 #
###################################################################################################################