####################
###### global ######
####################

library(dplyr)
#devtools::install_github("testthat", "hadley")

# Attributs de la data frame principale
varsMerchant <- c('siret', 'rs', 'date', 'montant', 'transaction', 'naf', 'groupe_naf', 'ville', 
                  'latitude', 'longitude', 'affilie_ca')

# Attributs de la data frame des transactions
varsTransactions <- c('montant_transaction', 'date_transaction', 'jour_transaction', 'mois_transaction', 'annee_transaction',
                      'heure_transaction', 'numero_siret', 'raison_sociale', 'naf', 'groupe_naf', 'ville_commercant', 'lat_commercant',
                      'long_commercant', 'affilie_ca', 'numer_carte', 'age_carte', 'debit', 'libelle_produit', 'reseau','paiement',
                      'retrait', 'niveau', 'id_compte', 'type_compte', 'credit_mensuel_total', 'debit_mensuel_total', 'solde', 'client',
                      'age', 'sexe', 'csp', 'opt_in_numero_portable', 'opt_in_mail', 'e_mail', 'nbr_enfants', 'situation_familiale',
                      'anciennete_relation', 'segment', 'score_de_risque', 'total_avoir', 'appetance_internet', 'appetance_mobile',
                      'ville_client', 'type_localite', 'lat_client', 'long_client', 'chiffre_affaire', 'montant_pret',
                      'montant_echeance_theorique', 'montant_restant_du_reel', 'date_1ere_echeance', 'date_derniere_echeance')

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

# Vecteurs des sous data frames de transactions 
matin <- c("matin")
midi <- c("midi")
pm <- c("pm")
soir <- c("soir")

# Récupération du nom de la table du commerçant connecté
getName <- function() {
  df.name <- paste("data/TransactionsByMerchant/", KEY$siret, ".txt", sep = "")
  return(df.name)
}