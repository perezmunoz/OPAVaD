####################
###### global ######
####################

vars <- c('siret', 'montant', 'rs', 'transaction', 'naf', 'groupe_naf', 'ville', 
          'latitude', 'longitude', 'affilie_ca')

commercants <- read.delim("data/merchants.txt", sep = "\t", header = FALSE, dec = ".", col.names = vars)
commercants$siret <- as.factor(commercants$siret)
commercants$latitude <- as.numeric(as.character(commercants$latitude))
commercants <- commercants[!is.na(commercants$latitude)
                           & !is.na(commercants$longitude)
                           & !is.na(commercants$rs)
                           & !is.na(commercants$groupe_naf), ]

MAPcolombier.data <- read.delim("data/MAPcolombier.txt", sep = "\t", header = TRUE, dec = ".")
