####################
###### global ######
####################

library(dplyr)
#devtools::install_github("testthat", "hadley")

# Attributs de la data frame principale
varsMerchant <- c('siret', 'rs', 'date', 'montant', 'transaction', 'naf', 'groupe_naf', 'ville', 
                  'latitude', 'longitude', 'affilie_ca')

# OBSOLETE
# # Attributs de la data frame des transactions
# varsTransactions <- c('montant_transaction', 'date_transaction', 'jour_transaction', 'mois_transaction', 'annee_transaction',
#                       'heure_transaction', 'numero_siret', 'raison_sociale', 'naf', 'groupe_naf', 'ville_commercant', 'lat_commercant',
#                       'long_commercant', 'affilie_ca', 'numer_carte', 'age_carte', 'debit', 'libelle_produit', 'reseau','paiement',
#                       'retrait', 'niveau', 'id_compte', 'type_compte', 'credit_mensuel_total', 'debit_mensuel_total', 'solde', 'client',
#                       'age', 'sexe', 'csp', 'opt_in_numero_portable', 'opt_in_mail', 'e_mail', 'nbr_enfants', 'situation_familiale',
#                       'anciennete_relation', 'segment', 'score_de_risque', 'total_avoir', 'appetance_internet', 'appetance_mobile',
#                       'ville_client', 'type_localite', 'lat_client', 'long_client', 'chiffre_affaire', 'montant_pret',
#                       'montant_echeance_theorique', 'montant_restant_du_reel', 'date_1ere_echeance', 'date_derniere_echeance')

# Attributs des data tables
var <- c('montant','date','heure','siret','rs','naf','villecom','lon','lat','affilie','client','age','sexe','csp','optel','opmail','mail',
         'enfant','situation','anciennete','segment','score','avoir','appinternet','appmobile','villeclient','uu','carte','libelle','reseau',
         'paiement','retrait', 'niveau','typecompte','credit','debit','mensualite','restant','datepremire','datederniere','vide','ca')

# Variables utiles à l'initialisation de la carte
north <- 0.00342
east <- 0.0204407
south <- 0.00271
west <- 0.0203933

# Données pour la map
commercants <- read.delim("data/dataMap.txt", sep = "\t", header = FALSE, dec = ".", col.names = varsMerchant)
commercants$siret <- as.character(commercants$siret)
commercants$date <- as.Date(commercants$date, format="%d/%m/%Y")
commercants <- commercants[!is.na(commercants$latitude)
                           & !is.na(commercants$longitude)
                           & !is.na(commercants$rs)
                           & !is.na(commercants$groupe_naf), ]

#####################################################################################################################

# var2 <- c('montant','date','heure','siret','rs','naf','villecom','lon','lat','affilie','client','age','sexe','csp','optel','opmail','mail',
# 'enfant','situation','anciennete','segment','score','avoir','appinternet','appmobile','villeclient','uu','carte','libelle','reseau',
# 'paiement','retrait', 'niveau','typecompte','credit','debit','mensualite','restant','datepremire','datederniere','vide','ca')
# 
# ## Colonnes que l'on garde pour construire la table
# # col <- c(1,2,4,5,6,7,8,9,10)
# col2 <- c('montant','date','siret','rs','naf','villecom','lon','lat','affilie')
# 
# # Chargement de la table sous forme de data table
# # df <- fread('C:/Users/CAvatar/Desktop/NAF/521D.txt', sep = "\t", select = col)
# # setnames(x = df, old=names(df), new = var)
# 
# df <- fread('C:/Users/CAvatar/Desktop/NAF/521D.txt', sep = "\t")
# setnames(x = df, old=names(df), new = var2)
# 
# # Struction des données
# # Temps d'exécution
# # utilisateur     système      écoulé 
# # 7.78        0.59        8.50 
# df$date <- with(df, as.IDate(x = date, format = "%d/%m/%Y"))
# 
# # Subset de la table pour retenir ce qui nous intéresse
# # Temps d'exécution
# # utilisateur     système      écoulé 
# # 0.38        0.03        0.40 
# tr <- subset(df, select=col2)
# 
# # Regroupement de la table
# # Temps d'exécution
# # utilisateur     système      écoulé 
# # 1.67        0.05        1.72 
# tr <- tr %>%
#   group_by(date,siret,rs,naf,villecom,lon,lat,affilie) %>%
#   summarise(montant=sum(montant),
#             transaction=n())

#####################################################################################################################

## Test avec la table de l'appli test ##
vars <- c('montant', 'date', 'siret', 'rs', 'naf', 'groupe_naf', 'ville', 
          'Lat', 'Long', 'client', 'age', 'sexe', 'csp', 'optin_tel', 'optin_mail', 'mail',
          'statut', 'anciennete', 'caisse', 'age_carte', 'avoir', 'ville_client')

# a = Sys.time()
# transactionsNAF <- read.delim("data/553B.txt", sep = "\t", header = FALSE, dec = ".", col.names = vars)
# transactionsNAF$siret <- as.character(transactionsNAF$siret)
# transactionsNAF$date <- as.Date(transactionsNAF$date, format="%d/%m/%Y")
# b = Sys.time()
# print(b-a)
# # Calcul du barycentre avec commercants.table
# commercants.table <- transactionsNAF[1:1000, ]

# INUTILE CAR TOUT EST REMPLACE AVEC L'OPTIMISATION
# Vecteurs des sous data frames de transactions 
matin <- c("matin")
midi <- c("midi")
pm <- c("pm")
soir <- c("soir")

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

## Calcul des barycentres des clients ##

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
  #   # e est la liste des transactions pour UN client
  print("bary")
  clients <- unique(e[ , 1])
  print(clients)
  table <- matrix(0 ,nrow = length(clients), ncol = 3)
  #print(table)
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
  #print(df.barycentres)
  return(df.barycentres)
}

# fidelisation <- reactive({
#   rangeDate <- getRangeDate()
#   print(rangeDate)
#   df <- transactionsNAF[transactionsNAF$date >= rangeDate[1]
#                         & transactionsNAF$date <= rangeDate[2], ]
#   df <- df[1:150, ]
#   if(nrow(df) == 0) {
#     return()
#   }
#   # on regroupe les transactions par identifiant client, raison sociale du commerçant ainsi que sa géolocalisation
#   resume <<- df %>%
#     group_by("client", "siret") %>%
#     summarise(n = n())
#   clients <- unique(resume[ , 1])
#   print(clients)
#   table <- matrix(ncol = 2)
#   fidelite <<- as.data.frame(table)
#   #   value <<- reactiveValues(fidelite = as.data.frame(table))
#   names(fidelite) <<- c("client", "type")
#   fidelite$client <<- as.character(fidelite$client)
#   fidelite$type <<- as.character(fidelite$type)
#   # On réalise autant d'itérations qu'il y a de clients
#   print("#####################################")
#   print(length(clients))
#   print("#####################################")
#   for(i in 1:length(clients)) {
#     print(paste("i =", i, "client n°", clients[i]))
#     # On récupère les consommations d'un seul client
#     determination <- subset(resume, clients[i] == resume$client)
#     # Si le client n'a pas fait d'achats chez le commerçant connecté ...
#     if(nrow(determination[determination$siret == KEY$siret, ]) == 0) {
#       # Alors c'est un prospect (s'il apparaît dans resume c'est parcequ'il a au moins réalisé un achat dans ce type de NAF)
#       fidelite <<- rbind(fidelite, c(as.character(clients[i]), "Prospects"))
#       # S'il a consommé chez le commerçant connecté et ailleurs, alors il est infidèle
#     } else if(nrow(determination[determination$siret == KEY$siret, ]) >= 1
#               & nrow(determination[determination$siret != KEY$siret, ]) >= 1) {
#       fidelite <<- rbind(fidelite, c(as.character(clients[i]), "Infidèles"))
#       # Sinon il est fidèle
#     } else if(nrow(determination[determination$siret == KEY$siret, ]) >= 1
#               & nrow(determination[determination$siret != KEY$siret, ]) == 0)
#     {fidelite <<- rbind(fidelite, c(as.character(clients[i]), "Fidèles"))}
#   }
#   fidelite <<- fidelite[-1, ]
#   print(fidelite)
#   return(fidelite)
# })

