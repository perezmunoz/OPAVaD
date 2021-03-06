####################
###### server ######
####################

library(shiny)
library(shinyBS)
library(ggvis)
library(leaflet)
library(ggplot2)
library(data.table)
library(bit64)
library(plyr)
library(dplyr)
library(reshape2)
library(parallel)
library(doParallel)
library(foreach)

Logged = FALSE;

shinyServer(function(input, output, session) {
  
  # Appel de l'interface de connexion
  source('www/login.r',  local = TRUE)
  
  # Observateur lisant l'intégralité du code lors de l'appui sur le bouton de connexion
  observe({
    # Encapsuleur donnant accès à l'application sous condition que la connexion soit validée
    if (USER$Logged == TRUE) {
      
      # Construction des élements du fond de commerce
      source('www/map.r', local = TRUE)
      
      # Chargement de la data table des transactions du commerçant connecté
      df.s <<- fread(getNameSIRET(), sep = "\t")
      setnames(x = df.s, old=names(df.s), new = var)
      
      # Chargement de la data table des transactions NAF selon le commerçant connecté  
      df.n <<- fread(getNameNAF(), sep = "\t")
      setnames(x = df.n, old=names(df.n), new = var.n)
      
      # Chargement de la data table du résumé des transactions des commerçants d'un même NAF
      df.r <<- fread(paste("C:/Users/CAvatar/Desktop/MAP/", KEY$naf, ".txt", sep = ""), sep = "\t")
      setnames(x = df.r, old=names(df.r), new = varMap)
      
      # Structuration de la data table
      df.r$siret <<- as.character(df.r$siret)
      df.r$date <<- as.IDate(df.r$date, format = "%d/%m/%Y")
      
      # Chargement de la data table des transactions du commerçant connecté pour le calcul du panier
      df.a <<- subset(df.s, select = c('montant','date','client','age','sexe','csp','situation','villeclient'))
      
      # Structuration de la data table
      df.a$date <<- as.IDate(df.a$date, format = "%d/%m/%Y")
      
      # Coordonnées géospatiales du commerçant transformées en radians (calcul de la prospection)
      latC <<- KEY$lat * pi/180
      lonC <<- KEY$lon * pi/180
      
      # Créaction d'un 'proxy' de la carte à l'aide duquel les cercles seront construits
      map <- createLeafletMap(session, 'map')
      
      # Méthode récupérant les données 'carte' lorsque la période de visualisation est modifiée
      # Concrètement, résume les données pré traitées en une seule ligne par commerçant
      getData <- function() {
        
        rangeDate <- getRangeDate()
        
        dat <- df.r[df.r$date >= rangeDate[1]
                    & df.r$date <= rangeDate[2], ]
        dat <- dat[order(dat$siret), ]
        dat <-group_by(dat, siret, rs, naf, ville, lat, lon, ca)
        dat <- summarise(.data = dat, montant = sum(montant), transaction = sum(transaction))
        dat
      }
      
      # Méthode récupérant les dates de début et fin de visualisation des données
      getRangeDate <- reactive({
        if(is.null(input$range[1]) & is.null(input$range[2])){
          out <- c(as.IDate("2013-05-01"), as.IDate("2013-05-08"))
        } else {
          out <- c(input$range[1], input$range[2])
        }
      })
      
      # Méthode renvoyant le nom du type de comparaison souhaité : par montant ou par transaction
      compareCol <- reactive({
        if (is.null(input$compareMap))
          return("montant")
        if (input$compareMap == "Montants") {
          return("montant")
        } else {return("transaction")}
      })
      
      makeReactiveBinding('selectedCom')
      
      # Méthode récupérant les commerçants sur le champ de vision de la carte
      comInBounds <- reactive({
        
        # Au lancement
        if(is.null(input$map_bounds)) {
          latRng <- c(KEY$lat + north, KEY$lat - south)
          lngRng <- c(KEY$lon - west, KEY$lon + east)
          
          inBounds <- subset(getData(),
                             lat >= latRng[1] & lat <= latRng[2]
                             & lon >= lngRng[1] & lon <= lngRng[2])
          return(inBounds[order(inBounds[[compareCol()]], decreasing = TRUE), ])
        } else {
          # Une fois l'application qui tourne
          bounds <- input$map_bounds
          latRng <- range(bounds$north, bounds$south)
          lngRng <- range(bounds$east, bounds$west)
          inBounds <- subset(getData(),
                             lat >= latRng[1] & lat <= latRng[2]
                             & lon >= lngRng[1] & lon <= lngRng[2])
          return(inBounds[order(inBounds[[compareCol()]], decreasing = TRUE), ])
        }
      })
      
      # Observation des clicks sur la carte. Si une pop-up était visible, le click sur un point quelconque 
      # de la carte (hors commerçants) ferme la pop-up actuelle en réinitialisant le commerçant sélectionné (selectedCom)   
      observe({
        if (is.null(input$map_click))
          return()
        selectedCom <<- NULL
      }) # Fin observateur 'map_click'
      
      # Observe la navigation de l'utilisateur sur la carte et met à jour l'affichage des commerçants
      observe({
        map$clearShapes()
        
        # Récupération des commerçants à afficher sur la carte
        listeCommercants <<- comInBounds()
        
        if (nrow(listeCommercants) == 0)
          return()
        
        # Ajout des cercles sur la carte
        map$addCircle(
          listeCommercants$lat,
          listeCommercants$lon,
          4 * sqrt(listeCommercants[[compareCol()]]) / (0.2 * input$map_zoom),
          row.names(listeCommercants),
          list(
            weight = 1.2,
            fill = TRUE,
            color = getColor())
        )
      }) # Fin observateur pour la construction des cercles sur le plan
      
      # Colorie le cercle des commerçants selon leur affiliation à CASA
      getColor <- function() {
        
        ids <- c(row.names(listeCommercants))
        category <- vector("list", nrow(listeCommercants))
        
        for(i in 1:nrow(listeCommercants)) {
          # Commerçants non affiliés
          if (listeCommercants[row.names(listeCommercants) == ids[i], ]$ca == 'N') {
            category[i] <- '#F03'
          } else {
            # Commerçant connecté
            if(listeCommercants[row.names(listeCommercants)== ids[i],]$siret == KEY$siret) {
              category[i] <- '#00F'
              # Commerçant non connecté mais affilié
            } else {category[i] <- '#4A9'}
          }
        }
        category
      }
      
      # Observateur pour la création des pop-up lors du click sur le cercle d'un commerçant
      observe({
        
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
            tags$strong("Affiliation au Crédit Agricole : "), sprintf("%s", com$ca), tags$br(),
            tags$strong("NAF : "), sprintf("%s", com$naf), tags$br(),
            tags$strong("Montant des transactions : "), sprintf("%s €", com$montant), tags$br(),
            tags$strong("Nombre de transactions : "), sprintf("%s", com$transaction), tags$br()
          ))
          map$showPopup(event$lat, event$lng, content, event$id)
        })
      }) # Fin observateur pour la construction des pop-up
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #                                          DEBUT MODULE CA JOURNALIER                                             
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Titre du graphique "Distribution du chiffre d'affaire selon la période de la journée"
      output$caTitre <- renderUI({
        tagList(tags$h4("Distribution du chiffre d'affaire selon la période de la journée"))
      })
      
      # Graphique "Distribution du chiffre d'affaire selon la période de la journée"
      output$graphiqueCA <- renderPlot ({
        
        session$sendCustomMessage(type='jsCode', list(value = "$('div.login').remove();"))
        
        # Structuration des données du commerçant KEY
        tr <- df.s
        tr$date <- with(tr, as.IDate(x = date, format = "%d/%m/%Y")) 
        tr$heure <- with(tr, as.ITime(x = heure, format = "%H:%M:%S"))
        
        # Récupération de la période de visualisation        
        periode <- getRangeDate()        
        tr <- tr[date %between% c(periode[1],periode[2])]
        
        # Tables selon la période de la journée. La séquence générée est par défaut par secondes
        matin <- tr[heure %in% seq(as.ITime("08:00:00", format = "%H:%M:%S"), as.ITime("12:00:00", format = "%H:%M:%S")), ]
        midi <- tr[heure %in% seq(as.ITime("12:00:01", format = "%H:%M:%S"), as.ITime("14:00:00", format = "%H:%M:%S")), ]
        pm <- tr[heure %in% seq(as.ITime("14:00:01", format = "%H:%M:%S"), as.ITime("17:00:00", format = "%H:%M:%S")), ]
        soir <- rbindlist(list(tr[heure %in% seq(as.ITime("17:00:01", format = "%H:%M:%S"), as.ITime("23:59:59", format = "%H:%M:%S")), ],
                               tr[heure %in% seq(as.ITime("00:00:00", format = "%H:%M:%S"), as.ITime("07:59:59", format = "%H:%M:%S")), ]))
        
        # La classe IDate contient un attribut qui ordonne automatiquement les données
        matin.s <- matin %>%
          group_by(date) %>%
          summarise(montant = sum(montant),
                    transaction = n())
        # Ajout de la dernière colonne pour le tracé
        matin.s[,period:="matin"]
        
        midi.s <- midi %>%
          group_by(date) %>%
          summarise(montant = sum(montant),
                    transaction = n())
        midi.s[,period:="midi"]
        
        pm.s <- pm %>%
          group_by(date) %>%
          summarise(montant = sum(montant),
                    transaction = n())
        pm.s[,period:="pm"]
        
        soir.s <- soir %>%
          group_by(date) %>%
          summarise(montant = sum(montant),
                    transaction = n())
        soir.s[,period:="soir"]
        
        # Fusion de l'ensemble des tables
        data.ca <- rbindlist(list(matin.s, midi.s, pm.s, soir.s))
        
        # Modification de la structure pour compatibilité avec la construction du graphique
        data.ca$period <- factor(data.ca$period, c("soir", "pm", "midi", "matin"))
        
        # Construction de la table de navigation principale et secondaire (petits onglets)
        session$sendCustomMessage(type='jsCode', list(value = "$('#opavad').parentsUntil('.navbar').parent().show();"))
        session$sendCustomMessage(type='jsCode', list(value = "$('#graphique-tab').show();"))
        
        # Traitement du use case : 'aucune transaction dans la période de visualisation' (exemple : visualisation sur une journée)
        if(nrow(data.ca)>0) {
          
          # MAJ du message d'erreur
          output$errorMessageCA <- renderUI({})
          
          # Définition du sectionnement journalier
          output$moncommerceExplication <- renderUI({
            br()
            tagList(
              tags$h5("La journée est découpée en quatre périodes clés pour mettre en valeur les habitudes de consommation des clients. Ces périodes sont définies comme suit :"),
              tags$ul(
                tags$li(HTML(paste(tags$b("le matin"), "définit entre 8h00 et 12h00"))),
                tags$li(HTML(paste(tags$b("le midi"), "définit entre 12h00 et 14h00"))),
                tags$li(HTML(paste(tags$b("l'après-midi"), "(alias pm) définit entre 14h00 et 17h00"))),
                tags$li(HTML(paste(tags$b("le soir"), "définit entre 17h00 et 8h00 du matin")))
              )
            )
          })
          
          # Construction et affichage du graphique
          ggplot(data = data.ca, aes_string(x = "date", y = compareCol())) +
            geom_bar(stat = 'identity', aes(fill = period)) +
            theme_bw() +
            labs(x = "Date de la transaction (mois/jour)", y = ifelse(compareCol()=="montant", "Montant des transactions (en €)", "Nombre de transactions")) +
            scale_fill_manual(values = c(matin="#99C0DB", midi="#FB998E", pm="#FDC381", soir="#C2E487")) +
            theme(axis.title.x = element_text(vjust = -0.5),
                  axis.title.y = element_text(vjust = 2),
                  panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank(), rect=element_blank())
          
          # Si la data.frame ne contient aucune transaction on affiche un message à l'utilisateur
        } else {
          output$moncommerceExplication <- renderUI({})
          output$errorMessageCA <- renderUI({tags$h5(paste("Aucune transaction entre le", input$range[1], "et le", input$range[2]))})
        }
      })
      
      # Définition du sectionnement journalier
      output$moncommerceExplication <- renderUI({
        br()
        tagList(
          tags$h5("La journée est découpée en quatre périodes clés pour mettre en valeur les habitudes de consommation des clients. Ces périodes sont définies comme suit :"),
          tags$ul(
            tags$li(HTML(paste(tags$b("le matin"), "définit entre 8h00 et 12h00"))),
            tags$li(HTML(paste(tags$b("le midi"), "définit entre 12h00 et 14h00"))),
            tags$li(HTML(paste(tags$b("l'après-midi"), "(alias pm) définit entre 14h00 et 17h00"))),
            tags$li(HTML(paste(tags$b("le soir"), "définit entre 17h00 et 8h00 du matin")))
          )
        )
      })
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #                                           FIN MODULE CA JOURNALIER                                              
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #                                        DEBUT MODULE PROFIL DES CLIENTS                                          
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Titre du graphique 'Répartition de la clientèle'
      output$clienteleTitre <- renderUI({
        tagList(tags$h4("Répartition de la clientèle"))
      })
      
      # Graphique 'Répartition de la clientèle'
      output$graphiqueClientele <- renderPlot({
        # Code JavaScript envoyé au serveur afin de modifier le fichier HTML généré.
        # On passe ainsi de <html class> à <html class='shiny-busy'> et donc busy.js prend la relève
        # L'utilisateur est donc prévenu de l'exécution des calculs en cours
        session$sendCustomMessage(type='jsCode', list(value = "$('html').attr('class','shiny-busy');"))
        
        fidelite <- fidelisation()
        
        # Si il a au moins un client fidèle ou infidèle alors on affiche les éléments associés
        if(fidelite[fidelite$type=="Fidèles",]$value!=0 & fidelite[fidelite$type=="Infidèles",]$value!=0) {

          session$sendCustomMessage(type='jsCode', list(value = "$($('hr')[2]).show()"))
          session$sendCustomMessage(type='jsCode', list(value = "$('div#descriptionRepartitionClientele').show()"))
          session$sendCustomMessage(type='jsCode', list(value = "$($('hr')[3]).show()"))
          session$sendCustomMessage(type='jsCode', list(value = "$('div#idclienteleSexeTitre').show()"))
          session$sendCustomMessage(type='jsCode', list(value = "$('div#graphiqueClienteleFidInfSexe').show()"))
          session$sendCustomMessage(type='jsCode', list(value = "$('div#idclienteleAgeTitre').show()"))
          session$sendCustomMessage(type='jsCode', list(value = "$('div#graphiqueClienteleFidInfAge').show()"))
          session$sendCustomMessage(type='jsCode', list(value = "$('div#idclienteleCSPTitre').show()"))
          session$sendCustomMessage(type='jsCode', list(value = "$('div#graphiqueClienteleFidInfCSP').show()"))
          session$sendCustomMessage(type='jsCode', list(value = "$('div#idclienteleSituationTitre').show()"))
          session$sendCustomMessage(type='jsCode', list(value = "$('div#graphiqueClienteleFidInfSituation').show()"))
          
          ggplot(aes(x = 1, y = value, fill = type), data = fidelite) +
            geom_bar(stat = 'identity', width = 1, colour = "white") +
            coord_polar(theta = "y") +
            guides(fill=guide_legend(override.aes=list(colour=NA))) +
            theme(legend.position = "bottom", axis.title=element_blank(), axis.text=element_blank(), axis.line=element_blank(), axis.ticks=element_blank(),
                  panel.background=element_blank(), panel.border=element_blank(), panel.grid=element_blank(),
                  legend.title=element_blank())
          # Sinon on cache tout        
        } else {
          session$sendCustomMessage(type='jsCode', list(value = "$($('hr')[2]).hide()"))
          session$sendCustomMessage(type='jsCode', list(value = "$('div#descriptionRepartitionClientele').hide()"))
          session$sendCustomMessage(type='jsCode', list(value = "$($('hr')[3]).hide()"))
          session$sendCustomMessage(type='jsCode', list(value = "$('div#idclienteleSexeTitre').hide()"))
          session$sendCustomMessage(type='jsCode', list(value = "$('div#graphiqueClienteleFidInfSexe').hide()"))
          session$sendCustomMessage(type='jsCode', list(value = "$('div#idclienteleAgeTitre').hide()"))
          session$sendCustomMessage(type='jsCode', list(value = "$('div#graphiqueClienteleFidInfAge').hide()"))
          session$sendCustomMessage(type='jsCode', list(value = "$('div#idclienteleCSPTitre').hide()"))
          session$sendCustomMessage(type='jsCode', list(value = "$('div#graphiqueClienteleFidInfCSP').hide("))
          session$sendCustomMessage(type='jsCode', list(value = "$('div#idclienteleSituationTitre').hide()"))
          session$sendCustomMessage(type='jsCode', list(value = "$('div#graphiqueClienteleFidInfSituation').hide()"))
          
          ggplot(aes(x = 1, y = value, fill = type), data = fidelite) +
            geom_bar(stat = 'identity', width = 1, colour = "white") +
            coord_polar(theta = "y") +
            guides(fill=guide_legend(override.aes=list(colour=NA))) +
            theme(legend.position = "bottom", axis.title=element_blank(), axis.text=element_blank(), axis.line=element_blank(), axis.ticks=element_blank(),
                  panel.background=element_blank(), panel.border=element_blank(), panel.grid=element_blank(),
                  legend.title=element_blank())
        }
      })
      
      # Construction de la table de fidélité (répartition prospects, clients fidèles et infidèles)
      fidelisation <- reactive({
        
        # Récupération de la période de visualisation et trigger de la fonction
        periode <- getRangeDate()
        
        # Si la data frame fidelite existe déjà on l'efface pour ne pas avoir de concurrence d'accès dans la suite des calculs
        if(exists("fidelite"))
          rm("fidelite", pos = ".GlobalEnv")
        
        # Recopiage de la table des transactions NAF en local
        tr <- df.n
        # Structuration de l'ensemble des transactions
        tr$siret <- as.factor(tr$siret)
        
        # Subset selon la période de visualisation
        tr <- tr[date %between% c(periode[1],periode[2])]
        
        # Récupération des 'reliques' transactionnelles
        tr.c <- unique(tr,by=c("client","siret"))[j=list(client,siret)]
        
        # On regroupe la table par client puis on test si le siret du commerçant est présent dans au moins une des transactions du client
        c.group <- group_by(tr.c, client)
        
        # On garde les clients ayant effectué au moins une transaction chez le commerçant connecté
        clients <- filter(c.group, KEY$siret %in% siret)
        
        # Si j'ai au moins un client potentiellement fidèle ou infidèle alors je les tag selon leur type de fidélité
        if(nrow(clients)>0) {
          
          clients <- summarise(clients, n=n())
          clients$fid <- numeric(length = nrow(clients))
          
          # On récupère les informations sur la fidélité des clients avant de supprimer la colonne
          fidelite <<- c(table(clients$n==1))
          
          # On tag les clients selon qu'ils sont des clients fidèles ou infidèles avec la variable fid
          # fid = 1 : clients fidèles
          # fid = 2 : clients infidèles
          for(i in 1:nrow(clients)) {
            if(clients$n[i]==1) {
              clients$fid[i]=1
            }
            else {clients$fid[i]=2}
          }
          clients$n <- NULL
        } else {
          clients$siret <- NULL
          clients$fid <- numeric()
          fidelite <<- numeric(length = 3)
        }
        
        # index.fidelite est la data table permettant de connaître le statut de fidélité des clients ayant effectué des achats durant la
        # période de visualisation des données
        # Pour mémo :
        # fid = 1 : clients fidèles
        # fid = 2 : clients infidèles
        # fid = 0 : prospects
        index.fidelite <- rbind.data.frame(data.frame(client = unique(c.group[!(client %in% clients$client)]$client),
                                                      fid = numeric(length = length(unique(c.group[!(client %in% clients$client)]$client)))),
                                           clients)
        
        # Notre data table pour la prospection est prête à l'emploi 
        df.p <<- merge(tr, index.fidelite, by = "client")
        
        # On termine de construire la table de fidélité pour le camembert
        fidelite[3] <<- nrow(unique(tr,by=c("client")))-(sum(fidelite))
        fidelite <<- cbind(melt(fidelite), type = c("Infidèles","Fidèles","Prospects"))
        # Remplacement des NAs par des 0 (cf ci-dessous)
        fidelite$value[is.na(fidelite$value)] <<- 0
        return(fidelite)
      })
      
      # Définitions des termes client infidèle, fidèle et prospect
      output$clienteleDefinition <- renderUI({
        tagList(
          tags$ul(tags$h5("Quelques définitions pour comprendre le graphique ci-contre"),
                  tags$li(tags$b("Prospect :"), tags$h6("Client ne consommant pas dans votre commerce mais chez des commerçants de même NAF.")), 
                  tags$li(tags$b("Infidèle :"), tags$h6("Client consommant chez vous et chez d'autres commerçants de même NAF.")), 
                  tags$li(tags$b("Fidèle :"), tags$h6("Client consommant exclusivement dans votre commerce")))
        )
      })
      
      # Renvoie la table fidelite lorsque la période de visualisation est modifiée (astuce pour rendre fidelite réactive)
      fidelite.up <- reactive({
        getRangeDate()
        fidelite
      })
      
      # Affichage de la répartition de la clientèle
      output$repartitionClientele <- renderUI({
        fidelite <- fidelite.up()
        tagList(div(tags$h2(fidelite[fidelite$type=="Prospects",1]), tags$h5("Prospects")),
                div(tags$h2(fidelite[fidelite$type=="Infidèles",1]), tags$h5("Clients infidèles")),
                div(tags$h2(fidelite[fidelite$type=="Fidèles",1]), tags$h5("Clients fidèles"))
                # Exemple de code si on avait laissé les NAs (cf ci-dessus)
                # div(tags$h2(ifelse(is.na(fidelite[fidelite$type=="Fidèles",1]),0,fidelite[fidelite$type=="Fidèles",1])), tags$h5("Clients fidèles"))
        )
      })
      
      # Fonction récupérant l'information clientèle selon les dates de visualisation
      data.profil <- reactive({
        rangeDate <- getRangeDate()
        df.s$date <- as.IDate(df.s$date, format = "%d/%m/%Y")
        df.profil <- subset(df.s, date %between% c(rangeDate[1],rangeDate[2]))
        df.profil <- subset(df.profil, select = c("client","age","sexe","csp","situation"))
        df.profil <- unique(df.profil) %>%
          ddply(.(age,sexe,csp,situation), summarise, freq=length(age)) %>%
          arrange(desc(freq))
        df.profil$rec <- df.profil$freq / sum(df.profil$freq) * 100
        df.profil$rec.cum <- cumsum(df.profil$rec)
        df.profil
      })
      
      # Table des profils de consommation
      output$tableProfilsClients <- renderDataTable({
        data.profil()
      }, options = list(orderClasses = TRUE))
      
        # Description des camemberts de répartition des clients
        output$repartitionClienteleCategories <- renderUI({
          tagList(
            tags$h5("Ci-dessous les graphiques donnant la répartition des clients fidèles et infidèles selon le sexe, l'âge, la catégorie
                  socio-professionnelle (csp) et la situation familiale.")
          )
        })
        
        output$modalUIProfil <- renderUI({
          bsModal("modProfil", "Profil des clients", trigger = "btnModalProfil",
                  tags$p("La table suivante établit les profils de consommation par ordre décroissant. Leur poids ainsi que le pourcentage cumulé est donné à titre indicatif."),
                  br(),
                  tags$div(class = "row-fluid",
                           tags$div(class = "span12",
                                    dataTableOutput("tableProfilsClients"))
                  )
          )
        })
        
        # Titre du graphique 'Répartition de la clientèle selon le sexe'
        output$clienteleSexeTitre <- renderUI({
          tagList(tags$h4("Répartition selon le sexe"))
        })
        
        # Graphique 'Répartition des clients selon le sexe'
        output$graphiqueClienteleFidInfSexe <- renderPlot({
          df.profil.sexe <- as.data.frame(data.profil() %>% group_by(sexe) %>% summarise(n = n()))
          
          ggplot(aes(x = 1, y = n, fill = sexe), data = df.profil.sexe) +
            geom_bar(stat = "identity", width = 1, colour = "white") +
            scale_fill_manual(values = c("M"="#0057e7", "F"="#d62d20"),
                              breaks = c("M", "F"),
                              labels = c(paste(round(df.profil.sexe[df.profil.sexe$sexe=="M",2]/sum(df.profil.sexe$n)*100,1), "% - ", "Homme", sep = ""),
                                         paste(round(df.profil.sexe[df.profil.sexe$sexe=="F",2]/sum(df.profil.sexe$n)*100,1), "% - ", "Femme", sep = ""))) +
            coord_polar(theta = "y") +
            guides(fill=guide_legend(override.aes=list(colour=NA), nrow = 2)) +
            theme(legend.position = "bottom", axis.title=element_blank(), axis.text=element_blank(), axis.line=element_blank(), axis.ticks=element_blank(),
                  panel.background=element_blank(), panel.border=element_blank(), panel.grid=element_blank(),
                  legend.title=element_blank())
        })
        
        # Titre du graphique 'Répartition de la clientèle selon l'âge'
        output$clienteleAgeTitre <- renderUI({
          tagList(tags$h4("Répartition selon l'âge"))
        })
        
        # Graphique 'Répartition des clients selon l'âge'
        output$graphiqueClienteleFidInfAge <- renderPlot({
          df.profil.age <- data.profil()
          df.profil.age <- data.frame(pallier = c("jeunes","adultes","senior","retraite"),
                                      freq = c(as.numeric(table(df.profil.age$age %between% c(0,25))[2]),
                                               as.numeric(table(df.profil.age$age %between% c(26,38))[2]),
                                               as.numeric(table(df.profil.age$age %between% c(39,49))[2]),
                                               as.numeric(table(df.profil.age$age %between% c(50,130))[2])))
          ## Alternative pour remplacer les NA par des 0. Le hic c'est que toutes les légendes apparaissent alors sur le graphique (ie celles avec 0% inutiles...)
          # df.profil.age$freq[is.na(df.profil.age$freq)] <- 0
          
          # Si il y a au moins un client fidèle ou infidèle
          if(length(na.omit(df.profil.age$freq))>0) {
            
            ggplot(aes(x = 1, y = freq, fill = pallier), data = df.profil.age) +
              geom_bar(stat = "identity", width = 1, colour = "white") +
              scale_fill_manual(values = c(jeunes="#008744",adultes="#0057e7",senior="#d62d20",retraite="#ffa700"),
                                breaks = c("jeunes","adultes","senior","retraite"),
                                labels = c(paste(round(df.profil.age$freq[1]/sum(df.profil.age$freq[!is.na(df.profil.age$freq)])*100,1), "% - ", "Moins de 25 ans", sep = ""),
                                           paste(round(df.profil.age$freq[2]/sum(df.profil.age$freq[!is.na(df.profil.age$freq)])*100,1), "% - ", "26-38 ans", sep = ""),
                                           paste(round(df.profil.age$freq[3]/sum(df.profil.age$freq[!is.na(df.profil.age$freq)])*100,1), "% - ", "39-50 ans", sep = ""),
                                           paste(round(df.profil.age$freq[4]/sum(df.profil.age$freq[!is.na(df.profil.age$freq)])*100,1), "% - ", "+50 ans", sep = ""))) +
              coord_polar(theta = "y") +
              guides(fill=guide_legend(override.aes=list(colour=NA), nrow = 2)) +
              theme(legend.position = "bottom", axis.title=element_blank(), axis.text=element_blank(), axis.line=element_blank(), axis.ticks=element_blank(),
                    panel.background=element_blank(), panel.border=element_blank(), panel.grid=element_blank(),
                    legend.title=element_blank())
          } else {return()}
        })
        
        # Titre du graphique 'Répartition de la clientèle selon la csp'
        output$clienteleCSPTitre <- renderUI({
          tagList(tags$h4("Répartition selon la csp"))
        })
        
        # Graphique 'Répartition des clients selon la csp'
        output$graphiqueClienteleFidInfCSP <- renderPlot({
          df.profil.csp <- as.data.frame(data.profil() %>% group_by(csp) %>% summarise(n = n()))
          
          ggplot(aes(x = 1, y = n, fill = csp), data = df.profil.csp) +
            geom_bar(stat = "identity", width = 1, colour = "white") +
            scale_fill_manual(values = c("#008744","#0057e7","#d62d20","#ffa700","#eeeeee","#433e90","#a19c9c","#6fcb9f"),
                              breaks = c("AGRICULTEURS","ARTISANTS COMMERCANTS ET CHEFS D'ENTREPRISES","AUTRES SANS ACTIVITE PROF.","CADRES ET PROF INTELLECTUELLES",
                                         "EMPLOYES","OUVRIERS","PROFESSIONS INTERMEDIAIRES","RETRAITES"),
                              labels = c(paste(round(df.profil.csp[df.profil.csp$csp=="AGRICULTEURS",]$n/sum(df.profil.csp$n)*100,1), "% - ", "Agriculteurs", sep = ""),
                                         paste(round(df.profil.csp[df.profil.csp$csp=="ARTISANTS COMMERCANTS ET CHEFS D'ENTREPRISES",]$n/sum(df.profil.csp$n)*100,1), "% - ", "Artisants, Commerçants \net Chefs d'Entreprises", sep = ""),
                                         paste(round(df.profil.csp[df.profil.csp$csp=="AUTRES SANS ACTIVITE PROF.",]$n/sum(df.profil.csp$n)*100,1), "% - ", "Autres sans activité \nprofessionnelle", sep = ""),
                                         paste(round(df.profil.csp[df.profil.csp$csp=="CADRES ET PROF INTELLECTUELLES",]$n/sum(df.profil.csp$n)*100,1), "% - ", "Cadres et \nProf. intellectuelles", sep = ""),
                                         paste(round(df.profil.csp[df.profil.csp$csp=="EMPLOYES",]$n/sum(df.profil.csp$n)*100,1), "% - ", "Employés", sep = ""),
                                         paste(round(df.profil.csp[df.profil.csp$csp=="OUVRIERS",]$n/sum(df.profil.csp$n)*100,1), "% - ", "Ouvriers", sep = ""),
                                         paste(round(df.profil.csp[df.profil.csp$csp=="PROFESSIONS INTERMEDIAIRES",]$n/sum(df.profil.csp$n)*100,1), "% - ", "Professions \nintermédiaires", sep = ""),
                                         paste(round(df.profil.csp[df.profil.csp$csp=="RETRAITES",]$n/sum(df.profil.csp$n)*100,1), "% - ", "Retraités", sep = ""))) +
            coord_polar(theta = "y") +
            guides(fill = guide_legend(override.aes = list(colour = NA), nrow = 4)) +
            theme(legend.position = "bottom", axis.title=element_blank(), axis.text=element_blank(), axis.line=element_blank(), axis.ticks=element_blank(),
                  panel.background=element_blank(), panel.border=element_blank(), panel.grid=element_blank(),
                  legend.title=element_blank())
        })
        
        # Titre du graphique 'Répartition de la clientèle selon la situation
        output$clienteleSituationTitre <- renderUI({
          tagList(tags$h4("Répartition selon la situation"))
        })
        
        # Graphique 'Répartition des clients selon la situation'
        output$graphiqueClienteleFidInfSituation <- renderPlot({
          df.profil.situation <- as.data.frame(data.profil() %>% group_by(situation) %>% summarise(n = n()))
          
          ggplot(aes(x = 1, y = n, fill = situation), data = df.profil.situation) +
            geom_bar(stat = "identity", width = 1, colour = "white") +
            scale_fill_manual(values = c("#008744","#0057e7","#d62d20","#ffa700","#eeeeee","#433e90","#a19c9c","#6fcb9f"),
                              breaks = c("CELIBATAIRE","CONCUBIN","DIVORCE","INIT","MARIE","PACSE","SEPARE","VEUF"),
                              labels = c(paste(round(df.profil.situation[df.profil.situation$situation=="CELIBATAIRE",]$n/sum(df.profil.situation$n)*100,1), "% - ", "Célibataire", sep = ""),
                                         paste(round(df.profil.situation[df.profil.situation$situation=="CONCUBIN",]$n/sum(df.profil.situation$n)*100,1), "% - ", "Concubin", sep = ""),
                                         paste(round(df.profil.situation[df.profil.situation$situation=="DIVORCE",]$n/sum(df.profil.situation$n)*100,1), "% - ", "Divorcé", sep = ""),
                                         paste(round(df.profil.situation[df.profil.situation$situation=="INIT",]$n/sum(df.profil.situation$n)*100,1), "% - ", "Init", sep = ""),
                                         paste(round(df.profil.situation[df.profil.situation$situation=="MARIE",]$n/sum(df.profil.situation$n)*100,1), "% - ", "Marié", sep = ""),
                                         paste(round(df.profil.situation[df.profil.situation$situation=="PACSE",]$n/sum(df.profil.situation$n)*100,1), "% - ", "Pacsé", sep = ""),
                                         paste(round(df.profil.situation[df.profil.situation$situation=="SEPARE",]$n/sum(df.profil.situation$n)*100,1), "% - ", "Séparé", sep = ""),
                                         paste(round(df.profil.situation[df.profil.situation$situation=="VEUF",]$n/sum(df.profil.situation$n)*100,1), "% - ", "Veuf", sep = ""))) +
            coord_polar(theta = "y") +
            guides(fill=guide_legend(override.aes=list(colour=NA), nrow = 4)) +
            theme(legend.position = "bottom", axis.title=element_blank(), axis.text=element_blank(), axis.line=element_blank(), axis.ticks=element_blank(),
                  panel.background=element_blank(), panel.border=element_blank(), panel.grid=element_blank(),
                  legend.title=element_blank())
        })
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #                                       FIN MODULE PROFIL DES CLIENTS                                             
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #                                      DEBUT MODULE PANIER DES CLIENTS                                            
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      #       updateCritere <- function(){
      #         # Si la table est mauvaise on réinitialise les critères avec ceux ayant conduit à une bonne table (crit.panier)
      #         updateSelectInput(session, inputId = "sexe",
      #                           label = "Sexe",
      #                           choices = c(Choisir = '', "M", "F"),
      #                           selected = crit.panier$sexeClients)
      #         
      #         updateSelectInput(session, inputId = "csp",
      #                           label = "Catégorie Socioprofessionnelle",
      #                           choices = c(Choisir = '', "AGRICULTEURS", "ARTISANTS COMMERCANTS ET CHEFS D'ENTREPRISES", "AUTRES SANS ACTIVITE PROF.",
      #                                       "CADRES ET PROF INTELLECTUELLES", "EMPLOYES", "OUVRIERS", "PROFESSIONS INTERMEDIAIRES", "RETRAITES"),
      #                           selected = crit.panier$cspClients)
      #         
      #         updateSelectInput(session, inputId = "situation",
      #                           label = "Situation familiale",
      #                           choices = c(Choisir = "", "CELIBATAIRE", "CONCUBIN", "DIVORCE", "INIT",
      #                                       "MARIE", "PACSE", "SEPARE", "VEUF"),
      #                           selected = crit.panier$situationClients)
      #       }
      
      # Fonction récupérant la valeur de l'input date. Evite message d'erreur visible par l'utilisateur
      getInputDate <- function() {
        if(is.null(input$date.vis)) {
          return(input$range[2])
        } else {return(input$date.vis)}
      }
      
      # Fonction récupérant la valeur de l'input sexe. Evite message d'erreur visible par l'utilisateur
      getInputSexe <- function() {
        if(is.null(input$sexe)) {
          return("")
        } else {return(input$sexe)}
      }
      
      # Fonction récupérant la valeur de l'input csp. Evite message d'erreur visible par l'utilisateur
      getInputCSP <- function() {
        if(is.null(input$csp)) {
          return("")
        } else {return(input$csp)}
      }
      
      # Fonction récupérant la valeur de l'input situation. Evite message d'erreur visible par l'utilisateur
      getInputSituation <- function() {
        if(is.null(input$situation)) {
          return("")
        } else {return(input$situation)}
      }
      
      output$graphiquePanier <- renderPlot({
        # On récupère les critères nouveaux au fur et à mesure qu'ils sont entrés par l'utilisateur
        crit.new <- data.frame(dateClients = c(input$date.vis),
                               sexeClients = c(input$sexe),
                               cspClients = c(input$csp),
                               situationClients = c(input$situation))
        
        # On récupère dans une nouvelle table les transactions pour un tel jour et les montants positifs
        # Les montants négatifs correspondant à des retours ou autres on ne les considère pas
        inputDate <- getInputDate()
        df.new <- subset(df.a, date == inputDate & montant > 0)
        
        # On filtre les données selon le sexe, la CSP, la situation et l'âge
        inputSexe <- getInputSexe()
        if(inputSexe!="") {
          df.new <- subset(df.new, sexe == input$sexe)
        }
        inputCSP <- getInputCSP()
        if(inputCSP!="") {
          df.new <- subset(df.new, csp == input$csp)
        }
        inputSituation <- getInputSituation()
        if(inputSituation!="") {
          df.new <- subset(df.new, situation == input$situation)
        }
        df.new <- subset(df.new, age %between% input$age)
        
        if(nrow(df.new) >= 3) {
          # ie que la nouvelle table créée est bonne pour printer sa densité, on la stocke pour une utilisation ultérieure
          df.panier <<- df.new
          crit.panier <<- crit.new
          
          # MAJ du sous-titre donnant le nombre de transactions visualisées
          output$panierSousTitre <- renderUI({tags$h5(paste("(visualisation sur", nrow(df.panier), "transactions)"))})
          # MAJ du message d'erreur 'pas assez de transactions...'
          output$errorPanierTitre <- renderUI({})
          # MAJ (effacement) de la table apparaissant lorsqu'il n'y a pas assez de transactions
          output$tableTransctionsPanierSansGraphique <- renderDataTable({})
          
          ggplot(df.panier, aes(x = montant)) +
            geom_density(alpha = 0.5, adjust = input$bin, colour = "white", fill = "#006700") +
            theme(axis.title.x = element_text(vjust = -0.5),
                  axis.title.y = element_text(vjust = 2),
                  panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), rect = element_blank())
          # Si la table n'est pas valide (au plus 2 transactions)
        } else {
          # MAJ (effacemnt) du sous-titre panierTitre
          output$panierSousTitre <- renderUI({})
          # MAJ du graphique en créant un plot vide
          plot.new()
          # MAJ du message d'erreur selon deux cas de figure
          # Cas 1 : si la table contient au moins une transaction on affiche la table
          if(nrow(df.new)>0) {
            # MAJ du message d'erreur
            output$errorPanierTitre <- renderUI({HTML(paste(tags$h5("Il n'y a pas assez de transactions pour permettre la visualisation du graphique selon les critères sélectionnés. Au minimum, trois transactions sont nécessaires pour l'afficher.
                                                       Au total, ", nrow(df.new)," transaction(s) a/ont eu lieu le ", input$date.vis, ". Ci-dessous les détails.", sep = "")))})
            # MAJ de la table contenant soit 1 ou 2 transactions
            output$tableTransctionsPanierSansGraphique <- renderDataTable({
              subset(df.new, select = c("age", "sexe", "csp", "situation", "villeclient"))
            }, options = list(orderClasses = TRUE))
            # Cas 2 : la table est nulle (aucune transaction ce jour-là)
          } else {
            # MAJ du message d'erreur
            output$errorPanierTitre <- renderUI({
              tagList(HTML(paste(tags$h5("Aucune transaction a été effectué au ", input$date.vis, " pour les critères suivants :", sep = ""))),
                      tags$ul(tags$li(HTML(paste("de sexe", ifelse(inputSexe=="M"|input$sexe=="F", paste(tags$strong(inputSexe)), paste(tags$strong("masculin"), "ou", tags$strong("féminin")))))),
                              tags$li(HTML(paste("de CSP", ifelse(inputCSP!="", paste(tags$strong(inputCSP)), paste(tags$strong("quelconque")))))),
                              tags$li(HTML(paste("de situation familiale", ifelse(inputSituation!="", paste(tags$strong(input$situation)), paste(tags$strong("quelconque")))))),
                              tags$li(HTML(paste("de tranche d'âge", tags$strong(input$age[1]), "-", tags$strong(input$age[2]), "ans")))
                      ),
                      tags$h5("Le graphique ne peut être affiché car au minimum, trois transactions sont nécessaires pour l'afficher."))
            })
            # MAJ de la table des transactions
            output$tableTransctionsPanierSansGraphique <- renderDataTable({})
          }
        }
      })
      
      # Fonction forçant l'apparition de la fenêtre 'modal' sans trigger
      fenetreModal <- function(){
        toggleModal(session, "modPanier")
        toggleModal(session, "modProfil")
      }
      
      output$modalUIPanier <- renderUI({
        # On récupère en variable locales les critères de l'utilisateur
        # L'écrire en dur dans HTML(...) est buggé
        feinterReactivite()
        
        if(nrow(df.panier)>0) {
          bsModal("modPanier", "Profil des clients", trigger = "btnModalPanier",
                  tags$p("Ci-dessous la table référençant le profil des clients ayant effectué une (ou plusieurs) transaction(s) le ",
                         tags$strong(crit.panier$dateClients), "et correspondant aux critères suivants :",
                         tags$ul(
                           tags$li(HTML(paste("de sexe", ifelse(crit.panier$sexeClients=="M"|crit.panier$sexeClients=="F", paste(tags$strong(crit.panier$sexeClients)), paste(tags$strong("masculin"), "ou", tags$strong("féminin")))))),
                           tags$li(HTML(paste("de CSP", ifelse(crit.panier$cspClients!="", paste(tags$strong(crit.panier$cspClients)), paste(tags$strong("quelconque")))))),
                           tags$li(HTML(paste("de situation", ifelse(crit.panier$situationClients!="", paste(tags$strong(crit.panier$situationClients)), paste(tags$strong("quelconque")))))),
                           tags$li(HTML(paste("âges entre", tags$strong(min(df.panier$age)), "et", tags$strong(max(df.panier$age)), "ans")))
                         ),
                         "Cette table contient les données du dernier graphique visualisé."
                  ),
                  br(),
                  tags$div(class = "row-fluid",
                           tags$div(class = "span12",
                                    dataTableOutput("transactionsClientsPrecis"))
                  )
          )
        } else {
          bsModal("modPanier", "Profil des clients", trigger = "btnModalPanier",
                  tags$p("Veuillez sélectionner une date de visualisation afin de visualiser les transactions du graphique correspondantes.")
          )
        }
      })
      
      # Table des transactions
      output$transactionsClientsPrecis <- renderDataTable({
        feinterReactivite()
        unique(subset(df.panier, select = c("age", "sexe", "csp", "situation", "villeclient")))
      }, options = list(orderClasses = TRUE))
      
      # Fonction forçant la mise à jour des tables visualisées à l'aide d'un bouton modal
      feinterReactivite <- reactive({
        date <- input$date.vis
        sexe <- input$sexe
        csp <- input$csp
        situation <- input$situation
        age <- input$age
      })
      
      # Astuce pour pouvoir réinitialiser les variables 'input'
      output$resetable_input <- renderUI({
        # Le bouton reset est compteur. A chaque appui il est incrémenté. Initialitement il est NULL. On doit donc l'initialiser
        times <- input$reset_input
        
        # Initialisation du bouton reset_input (message d'erreur supprimé)
        if(is.null(times)) {
          times <- 0
        }
        
        # %% : opérateur modulo
        div(id = letters[(times %% length(letters)) + 1],
            # Date de visualisation des données
            dateInput(inputId = "date.vis", label = "Date de visualisation du panier", value = "2013-05-08", language = "fr", min = "2012-09-28", max = "2013-10-02"),
            
            # Faire apparaître le choix du sexe
            selectInput("sexe",
                        label = "Sexe",
                        choices = c(Choisir = "", "M", "F"),
                        selectize = TRUE
            ),
            
            # Faire apparaître le choix de la csp
            selectInput("csp",
                        label = "Catégorie Socioprofessionnelle",
                        choices = c(Choisir = "", "AGRICULTEURS", "ARTISANTS COMMERCANTS ET CHEFS D'ENTREPRISES", "AUTRES SANS ACTIVITE PROF.",
                                    "CADRES ET PROF INTELLECTUELLES", "EMPLOYES", "OUVRIERS", "PROFESSIONS INTERMEDIAIRES", "RETRAITES"),
                        selectize = TRUE
            ),
            
            # Faire apparaître le choix de la situation
            selectInput("situation",
                        label = "Situation familiale",
                        choices = c(Choisir = "", "CELIBATAIRE", "CONCUBIN", "DIVORCE", "INIT",
                                    "MARIE", "PACSE", "SEPARE", "VEUF"),
                        selectize = TRUE
            ),
            
            # Faire apparaître le choix de l'âge
            sliderInput(inputId = "age",
                        label = "Tranche d'âge",
                        min = 15, max = 100, value = c(15,100))
        )
      })
      
      output$composantsReglagePanier <- renderUI({
        
        div(id = "adjust-modal-reset",
            # Réglage de la précision de la distribution
            sliderInput(inputId = "bin",
                        label = "Ajustement de la précision",
                        min = .1, max = 2, value = .5, step = .1),
            hr(),
            # Bouton affichant la table des transactions
            bsButton("btnModalPanier", "Données du graphique", style = "primary"),
            br(),
            # Bouton servan à réinitialiser les variables 'input'
            actionButton("reset_input", "Réinitialiser les critères")
        )
      })
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #                                       FIN MODULE PANIER DES CLIENTS                                             
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #                                        DEBUT MODULE DE PROSPECTION                                              
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Observateur pour la prospection : isole le bouton de lancement de la prospection (évite de charger les tables à nouveau)
      observe({
        
        # Affichage du nombre de clients atteints par la prospection
        output$prospectionObjectif <- renderUI({
          # si la prospection n'a pas encore été lancé, on prévient le commerçant de la lancer
          if(!exists("df.prospection")) {
            tagList(tags$h1("0"), tags$h3("Clients potentiels"),
                    tags$h3("Penser à visualiser la répartition de la fidélité des clients puis à lancer la prospection !")
            )
          } else {
            # Affichage des résultats de la prospection
            tagList(tags$h1(nrow(df.prospection[df.prospection$dist==1,])), tags$h3("Clients potentiels"),
                    tags$h1(ifelse(length(df.prospection$dist)!=0,paste(round(mean(df.prospection$panier),1), "€"),"0 €")), tags$h3("Panier moyen client"),
                    tags$h1(ifelse(length(df.prospection$dist)!=0,paste(nrow(df.prospection[df.prospection$dist==1,])*floor(mean(df.prospection$panier)), "€"), "0 €")), tags$h3("Bénéfices potentiels*")
            )
          }
        })
        
        # Affichage du nombre de prospects sur la période de visualisation
        output$prospectionRecapitulatifClientele <- renderUI({
          if(exists("fidelite")) {
            fidelite <- fidelite.up()
            tagList(div(tags$h2(fidelite[fidelite$type=="Prospects",1]), tags$h5("Prospects")),
                    div(tags$h2(fidelite[fidelite$type=="Infidèles",1]), tags$h5("Clients infidèles")),
                    div(tags$h2(fidelite[fidelite$type=="Fidèles",1]), tags$h5("Clients fidèles"))
            )
          }
        })
        
        output$criteresProspectionDesc <- renderUI ({
          # Récupération des variables de critères
          sexe <- input$checkBoxProspectionSexe
          age <- input$selectInputProspectionAge
          situation <- input$selectInputSituationProspection
          csp <- input$selectInputCSPProspection
          
          # Construction de la chaîne de caractère pour la situation
          situation.chaine <- listeCriteresProspection(situation)
          
          # Construction de la chaîne de caractère pour la CSP
          csp.chaine <- listeCriteresProspection(csp)
          
          tagList(
            tags$h5("Ci-dessous le récapitulatif des critères des clients sur lequel porte la prospection :"),
            tags$ul(
              tags$li(HTML(paste("de sexe", ifelse(length(sexe)==2, paste(tags$strong("masculin"), "et", tags$strong("féminin")), paste(tags$strong(sexe)))))),
              tags$li(HTML(paste("âgés entre", tags$strong(age[1]), "et", tags$strong(age[2]), "ans"))),
              tags$li(HTML(paste("de situation familiale", ifelse(length(situation)!=0, situation.chaine, paste(tags$strong("quelconque")))))),
              tags$li(HTML(paste("de CSP", ifelse(length(csp)!=0, csp.chaine, paste(tags$strong("quelconque"))))))
            ),
            HTML(paste("Par défaut, les", strong("clients fidèles"), "et les", strong("clients infidèles"), "sont prospectés. Cela reste paramétrable."))
          )
        })
        
        # Fonction construisant la chaîne de caractères pour les critères situation familiale et CSP
        listeCriteresProspection <- function(e) {
          if(length(e)!=0) {
            e.chaine <- ""
            if(length(e)==1) {
              e.chaine <- HTML(paste(e.chaine, tags$strong(e)))
            } else {
              for(i in 1:(length(e)-1)) {
                e.chaine <- HTML(paste(e.chaine, tags$strong(e[i]), ", ", sep = ""))  
              }
              e.chaine <- HTML(paste(e.chaine, tags$strong(e[length(e)])))
            }
          }
        }
        
        output$champActionProspectionTitre <- renderUI ({
          tagList(tags$h2("Périmètre d'action prospection"))
        })
        
        # Algorithme de prospection
        prospection <- reactive({
          
          # browser()()
          
          # Trigger pour l'affichage du panneau "Calcul en cours..."
          session$sendCustomMessage(type='jsCode', list(value = "$('html').attr('class','shiny-busy');"))
          
          # Est directement dépendant de l'action sur la bouton input$btnChampAction
          # Lorsque btnChampAction est appuyé, toutes les fonctions ci-dessous sont exécutées
          input$actionButtonLancerProspection
          
          # On isole la parallélisation du calcul de prospection afin qu'il ne soit exécuté uniquement lorsque le client le désire réellement 
          isolate({
            
            if(nrow(df.p)==0) {
              return()
            } else {
              # On subset la table de prospection pour ne garder les choix du commerçant
              df.prospection <- unique(df.p %>%
                                         group_by(client) %>%
                                         summarise(panier=mean(montant),
                                                   date=max(date),
                                                   age=age,
                                                   sexe=sexe,
                                                   csp=csp,
                                                   situation=situation,
                                                   villeclient=villeclient,
                                                   libelle=libelle,
                                                   reseau=reseau,
                                                   paiement=paiement,
                                                   retrait=retrait,
                                                   typecompte=typecompte,
                                                   Glat=Glat,
                                                   Glon=Glon,
                                                   fid=fid))
              
              # Les filtres ne s'appliquent qu'aux prospects
              # Si le commerçant décide de prospecter les clients fidèles et/ou infidèles alors ils ne sont pas comptabilisés
              # Cas où il veut prospecter que les clients infidèles : alors les clients fidèles sont soumis aux mêmes filtres que les prospects
              if(length(input$checkBoxFideliteProspection)!=0) {
                df.prospection.fideles <- cbind(subset(df.prospection, fid %in% input$checkBoxFideliteProspection), dist = c(1))
                df.prospection <- subset(df.prospection, !(fid %in% input$checkBoxFideliteProspection))
              }
              
              # On subset la data table contenant les clients à prospecter en fonction des critères du commerçant
              # D'abord on subset par sexe et âge
              df.prospection <- subset(df.prospection, sexe %in% input$checkBoxProspectionSexe
                                       & age %between% input$selectInputProspectionAge)
              
              # La condition if empêche le subset dans le cas où le critère n'a pas été précisé
              # Puis par situation
              if(length(input$selectInputSituationProspection)!=0) {
                df.prospection <- subset(df.prospection, situation %in% input$selectInputSituationProspection)
              }
              
              # Enfin par csp
              if(length(input$selectInputCSPProspection)!=0) {
                df.prospection <- subset(df.prospection, csp %in% input$selectInputCSPProspection)
              }
              
              # Pour mémo :
              # fid = 1 : clients fidèles
              # fid = 2 : clients infidèles
              # fid = 0 : prospects
              # Le vecteur input$checkBoxFideliteProspection contient les valeurs 1 et 2 selon le choix du commerçant
              
              # Data table finale
              # dist = 0 : n'est pas présent dans le champ d'action
              # dist = 1 : est présent dans le champ d'action
              
              # Condition if : le commerçant souhaite t-il prospecter nécessairement les clients fidèles et/ou infidèles ?
              df.prospection <<- if(length(input$checkBoxFideliteProspection)!=0) {
                # Si oui, la prospection porte t-elle EXCLUSIVEMENT sur ces clients ?
                if(nrow(df.prospection)!=0) {
                  # Si df.prospection est non nul, alors les critères sont tels que la prospection est porte aussi sur des cliens de fid = 0
                  rbind.data.frame(
                    # Résidu de prospection hors choix du commerçant sur les clients fidèles et infidèles
                    distanceCompute(df.prospection),
                    # Table contenant le choix du commerçant sur les clients fidèles et infidèles
                    df.prospection.fideles
                  )
                } else {
                  # Les critères sont tels qu'il n'y a pas de prospects
                  # On retourne seulement la table des fidèles/infidèles car nous savons qu'elle existe
                  df.prospection.fideles
                }
              } else {
                # Si le commerçant souhaite prospecter tout le monde indifférement, clients fidèles/infidèles et prospects
                if(nrow(df.prospection)!=0) {
                  # Si les critères font qu'il y a des prospects, alors on l'envoi dans distanceCompute
                  distanceCompute(df.prospection)
                }
              }
            }
            # Fin de l'isolement : supprime la dépendance de input$numericInputChampAction
          })
        })
        
        distanceCompute <- function(df) {
          cbind(df,
                # Variable catégorique donnant la présence ou non du client dans le champ d'action du commerçant                        
                dist = foreach(line = iter(df, by = 'row'), .combine = 'c', .packages = c('parallel','doParallel','foreach')) %do% {
                  val <- acos(sin(latC)*sin(line$Glat*pi/180)+cos(latC)*cos(line$Glat*pi/180)*cos(line$Glon*pi/180-lonC)) * rayon
                  if(!is.na(val)) {
                    ifelse(val <= input$numericInputChampAction, 1, 0)
                  } else {
                    print(line)
                    return(0)
                  }
                })
        }
        
        # Listener sur l'algorithme de prospection ci-desssus
        prospection()
        
      }) # Fin observateur 'propsection'
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #                                          FIN MODULE DE PROSPECTION                                              
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }
  }) # Fin observateur principal
})