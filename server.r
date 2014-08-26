####################
###### server ######
####################

library(shiny)
library(leaflet)
library(rCharts)
library(maps)
library(ggplot2)

Logged = FALSE;

shinyServer(function(input, output, session) {
  
  # Appel à l'interface de connexion
  source('www/login.r',  local = TRUE)
  
  observe({
    # Wrapper donnant accès à l'application sous condition que la connexion soit validée
    if (USER$Logged == TRUE) {
      
      print("[server.R] Connexion réussie")
      
      # Construction des élements du fond de commerce
      source('www/map.r', local = TRUE)
      print("map.r est 'sourcé'")
      
      # Chargement des transactions du commerçant connecté
      key.df.transactions <<- read.delim(getName(), sep = "\t", header = FALSE, dec = ".", col.names = varsTransactions)
      print("Table du commerçants connecté récupérées et non traitées")
      
      # Créaction d'un 'proxy' de la carte à l'aide duquel les cercles seront construits
      map <- createLeafletMap(session, 'map')
      print("map est créée")
      
      # Méthode récupérant les données pour la carte
      getData <<- reactive({
        
        rangeDate <- getRangeDate()
        
        print("[getData] Construction de la table")
        res <- commercants[commercants$date >= rangeDate[1]
                           & commercants$date <= rangeDate[2], ]
        res <- res[order(res$siret), ]
        res <- group_by(res, siret, rs, naf, groupe_naf, ville, latitude, longitude, affilie_ca)
        res <- summarise(res, montant = sum(montant), transaction = sum(transaction))
        print("[getData] Table commerçants construite")
        return(res)
      })
      
      # Méthode récupérant les dates de début et fin de visualisation des données
      getRangeDate <- function() {
        print("[getRangeDate] Entrée fonction")
        out <<- c(as.Date("2013-05-01"), as.Date("2013-05-20"))
        if(!is.null(input$range[1]) & !is.null(input$range[2])) {
          out[1] <- input$range[1]
          out[2] <- input$range[2]
          return(out)
        }
        return(out)
      }
      
      # Méthode renvoyant le nom du type de comparaison souhaité : par montant ou par transaction
      compareCol <<- reactive({
        print("[compareCol] Entrée fonction")
        if (is.null(input$compareMap))
          return("montant")
        if (input$compareMap == "Montants") {
          return("montant")
        } else {return("transaction")}
      })
      
      makeReactiveBinding('selectedCom')
      
      # Méthode récupérant les commerçants sur le champ de vision de la carte
      comInBounds <- reactive({
        print("[comInBounds] Entrée fonction")
        
        if(is.null(input$map_bounds)) {
          print("[comInBounds] input$map_bounds nul")
          latRng <- c(KEY$latitude + north, KEY$latitude - south)
          lngRng <- c(KEY$longitude - west, KEY$longitude + east)
          
          inBounds <- subset(getData(),
                             latitude >= latRng[1] & latitude <= latRng[2]
                             & longitude >= lngRng[1] & longitude <= lngRng[2])
          print("[comInBounds] table inBounds retournée")
          return(inBounds[order(inBounds[[compareCol()]], decreasing = TRUE), ])
        } else {
          print("[comInBounds] input$map_bounds non nul")
          bounds <- input$map_bounds
          latRng <- range(bounds$north, bounds$south)
          lngRng <- range(bounds$east, bounds$west)
          inBounds <- subset(getData(),
                             latitude >= latRng[1] & latitude <= latRng[2]
                             & longitude >= lngRng[1] & longitude <= lngRng[2])
          print("[comInBounds] table inBounds retournée")
          return(inBounds[order(inBounds[[compareCol()]], decreasing = TRUE), ])
        }
      })
      
      # Observe les clicks sur la carte. Si une pop-up était activtée, le click sur un point quelconque 
      # de la carte hors les commerçants ferme la pop-up actuelle en réinitialisant le commerçant sélectionné (selectedCom)   
      observe({
        if (is.null(input$map_click))
          print("[observe map_click] input$map_click est nul")
        return()
        selectedCom <<- NULL
      })
      
      # Observe la navigation de l'utilisateur sur la carte et met à jour l'affichage des commerçants
      observe({
        map$clearShapes()
        print("[observe circles] Création de la surcouche de la map (cercles)")
        
        listeCommercants <<- comInBounds()
        print("[observe circles] Commerçants inBounds pour le tracé de la map récupérés")
        
        if (nrow(listeCommercants) == 0)
          return()
        print("[observe circles] Construction des cercles")
        map$addCircle(
          listeCommercants$latitude,
          listeCommercants$longitude,
          4 * sqrt(listeCommercants[[compareCol()]]) / (0.5 * input$map_zoom),
          row.names(listeCommercants),
          list(
            weight = 1.2,
            fill = TRUE,
            color = getColor())
        )
      })
      
      # Colorie le cercle des commerçants selon leur affiliation à CASA
      getColor <- function() {
        
        print("[getColor] Obtention des couleurs")
        
        # Modification de la rowname de la KEY pour pouvoir réaliser les tests qui suivent
        if(nrow(listeCommercants[listeCommercants$siret == KEY$siret, ]) > 0) {
          row.names(KEY) <<- row.names(listeCommercants[listeCommercants$siret == KEY$siret, ]) 
        }
        ids <- c(row.names(listeCommercants))
        category <- vector("list", nrow(listeCommercants))
        for(i in 1:nrow(listeCommercants)) {
          if (listeCommercants[row.names(listeCommercants) == ids[i], ]$affilie_ca == 'O') {
            if (row.names(KEY) == ids[i]) {
              print("[getColor] Commerçant KEY colorié d'une autre couleur")
              category[i] <- '#00F'
            } else { print("[getColor] Commerçants non KEY et affiliés peints en vert")
                     category[i] <- '#4A9' }
          } else { print("[getColor] Commerçants non KEY et affiliés peints en rouge")
                   category[i] <- '#F03' }    
        }
        return(category)
      }
      
      # Observateur pour la création des pop-up lors du click sur le cercle d'un commerçant
      observe({
        print("[observe pop-up] Création des pop-up")
        # Récupération des infos sur l'object clické
        event <- input$map_shape_click
        if (is.null(event))
          return()
        # Si click sur un cercle, alors on efface les pop-up précédents et affiche celui correspondant
        map$clearPopups()
        
        isolate({
          com <- listeCommercants[row.names(listeCommercants) == event$id,]
          selectedCom <<- com
          content <- as.character(tagList(
            tags$strong(com$rs), tags$br(),
            tags$strong("Siret : "), sprintf("%s", com$siret), tags$br(),
            tags$strong("Affiliation au Crédit Agricole : "), sprintf("%s", com$affilie_ca), tags$br(),
            tags$strong("NAF : "), sprintf("%s", com$naf), tags$br(),
            tags$strong("Groupe NAF : "), sprintf("%s", com$groupe_naf), tags$br(),
            tags$strong("Montant des transactions : "), sprintf("%s €", com$montant), tags$br(),
            tags$strong("Nombre de transactions : "), sprintf("%s", com$transaction), tags$br()
          ))
          map$showPopup(event$lat, event$lng, content, event$id)
        })
      })
      # Graphique n°1
      output$ca <- renderPlot ({
        
        print("[output$ca] Construction de l'histogramme")
        
        # Structuration des données du commerçant KEY
        key.df.transactions$date_transaction <- as.Date(key.df.transactions$date_transaction, format="%d/%m/%Y")
        key.df.transactions$numero_siret <- as.character(key.df.transactions$numero_siret)
        key.df.transactions$heure_transaction <- strptime(key.df.transactions$heure_transaction, "%H:%M:%S")
        print("[output$ca] Modification str de la table du commerçant key.df.transactions terminée")
        # Initialisation des dates de visualisation
        # Empêche l'erreur Error : upper value must be greater than lower value 
        if(is.null(input$range)) {
          period = (out[2] - out[1]) + 1
        } else {period = (input$range[2] - input$range[1]) + 1}
        
        print(paste("[output$ca] Récupération de la période :", period))
        print(input$range[1])
        print(input$range[2])
        rangeDate <- getRangeDate()
        print(paste("Les input$range sont-ils nuls ?", is.null(input$range)))
        # Filtrage des transactions client selon les dates de visualisation
        selected <- key.df.transactions[key.df.transactions$date_transaction >= rangeDate[1]
                                        & key.df.transactions$date_transaction <= rangeDate[2], ]
        print("[output$ca] Table ky.df subset avec les input$range")
        selected <- selected[order(selected$date_transaction), ]
        # Table matin contenant le récapitulatif des transactions 'matin'
        matin.data <- selected[selected$heure_transaction >= strptime("08:00:00", "%H:%M:%S")
                               & selected$heure_transaction <= strptime("12:00:00", "%H:%M:%S"), ]
        matin.data$heure_transaction <- NULL
        matin.summary <- matin.data %.%
          group_by(date_transaction) %.%
          summarise(montant = sum(montant_transaction),
                    transaction = n())
        matin <- rep(matin, each = as.numeric(period))
        matin.summary$period <- matin
        print("[output$ca] Table key.df.matin construite")
        # Table matin contenant le récapitulatif des transactions 'midi'
        midi.data <- selected[selected$heure_transaction > strptime("12:00:00", "%H:%M:%S")
                              & selected$heure_transaction <= strptime("14:00:00", "%H:%M:%S"), ]
        midi.data$heure_transaction <- NULL
        midi.summary <- midi.data %.%
          group_by(date_transaction) %.%
          summarise(montant = sum(montant_transaction),
                    transaction = n())
        midi <- rep(midi, each = as.numeric(period))
        midi.summary$period <- midi
        print("[output$ca] Table key.df.midi construite")
        # Table matin contenant le récapitulatif des transactions 'pm'
        pm.data <- selected[selected$heure_transaction > strptime("14:00:00", "%H:%M:%S")
                            & selected$heure_transaction <= strptime("17:00:00", "%H:%M:%S"), ]
        pm.data$heure_transaction <- NULL
        pm.summary <- pm.data %.%
          group_by(date_transaction) %.%
          summarise(montant = sum(montant_transaction),
                    transaction = n())
        pm <- rep(pm, each = as.numeric(period))
        pm.summary$period <- pm
        print("[output$ca] Table key.df.pm construite")
        # Table matin contenant le récapitulatif des transactions 'soir'
        soir.data <- selected[(selected$heure_transaction > strptime("17:00:00", "%H:%M:%S")
                               & selected$heure_transaction < strptime("23:59:59", "%H:%M:%S"))
                              | (selected$heure_transaction >= strptime("00:00:00", "%H:%M:%S")
                                 & selected$heure_transaction < strptime("08:00:00", "%H:%M:%S")), ]
        soir.data$heure_transaction <- NULL
        soir.summary <- soir.data %.%
          group_by(date_transaction) %.%
          summarise(montant = sum(montant_transaction),
                    transaction = n())
        soir <- rep(soir, each = as.numeric(period))
        soir.summary$period <- soir
        print("[output$ca] Table key.df.soir construite")
        # Fusion des tables
        sel <- rbind(matin.summary, midi.summary)
        sel <- rbind(sel, pm.summary)
        fin <<- rbind(sel, soir.summary)
        print("[output$ca] Table fin construite")
        
        print(paste("[output$ca] Tracé selon :", compareCol()))
        
        print("[output$ca] ggplot en construction")
        return(ggplot(data = fin, aes_string(x = "date_transaction", y = compareCol())) + geom_bar(stat = 'identity', aes(fill = period)) + theme_bw() + 
                 theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank(), rect=element_blank()))
      })     
    }
  })
})