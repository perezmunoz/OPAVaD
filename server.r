####################
###### server ######
####################

require(shiny)
library(shinyBS)
library(ggvis)
require(leaflet)
require(ggplot2)
require(data.table)
require(bit64)
require(dplyr)
require(reshape2)

Logged = FALSE;

shinyServer(function(input, output, session) {
  
  # Appel de l'interface de connexion
  source('www/login.r',  local = TRUE)
  
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
      setnames(x = df.n, old=names(df.n), new = var)
      
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
      
      # Créaction d'un 'proxy' de la carte à l'aide duquel les cercles seront construits
      map <- createLeafletMap(session, 'map')
      
      # Méthode récupérant les données 'carte' lorsque la période de visualisation est modifiée
      # Concrètement, résume les données pré traitées en une seule ligne par commerçant
      getData <- function() {
        
        rangeDate <- getRangeDate()
        
        dat <- df.r[df.r$date >= rangeDate[1]
                           & df.r$date <= rangeDate[2], ]
        dat <- dat[order(dat$siret), ]
        dat <- group_by(x = dat, siret, rs, naf, ville, lat, lon, ca)
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
      })
      
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
      })
      
      # Titre du graphique "Distribution du chiffre d'affaire selon la période de la journée"
      output$caTitre <- renderUI({
        tagList(tags$h4("Distribution du chiffre d'affaire selon la période de la journée"))
      })
      
      # Graphique "Distribution du chiffre d'affaire selon la période de la journée"
      output$graphiqueCA <- renderPlot ({
        
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

        # Construction et affichage du graphique
        ggplot(data = data.ca, aes_string(x = "date", y = compareCol())) +
          geom_bar(stat = 'identity', aes(fill = period)) +
          theme_bw() +
          labs(x = "Date de la transaction (mois/jour)", y = ifelse(compareCol()=="montant", "Montant des transactions (en €)", "Nombre de transactions")) +
          scale_fill_manual(values = c(matin="#99C0DB", midi="#FB998E", pm="#FDC381", soir="#C2E487")) +
          theme(axis.title.x = element_text(vjust = -0.5),
                axis.title.y = element_text(vjust = 2),
                panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank(), rect=element_blank())
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
      
      # Titre du graphique 'Répartition de la clientèle'
      output$clienteleTitre <- renderUI({
        tagList(tags$h4("Répartition de la clientèle"))
      })
      
      # Graphique 'Répartition de la clientèle'
      output$graphiqueClientele <- renderPlot({
        ggplot(aes(x = 1, y = value, fill = type), data = fidelisation()) +
          geom_bar(stat = 'identity', width = 1, colour = "black") +
          coord_polar(theta = "y") +
          guides(fill=guide_legend(override.aes=list(colour=NA))) +
          theme(legend.position = "bottom", axis.title=element_blank(), axis.text=element_blank(), axis.line=element_blank(), axis.ticks=element_blank(),
                panel.background=element_blank(), panel.border=element_blank(), panel.grid=element_blank(),
                legend.title=element_blank())
      })
      
      # Construction de la table de fidélité (répartition prospects, clients fidèles et infidèles)
      fidelisation <- reactive({
        
        # Recopiage de la table des transactions NAF en local
        tr <- df.n
        
        # Structuration de l'ensemble des transactions
        tr$date <- with(tr, as.IDate(x = date, format = "%d/%m/%Y"))
        tr$siret <- as.factor(tr$siret)
        
        # Récupération de la période de visualisation
        periode <- getRangeDate()        
        tr <- tr[date %between% c(periode[1],periode[2])]
        
        # Récupération des 'reliques' transactionnelles
        tr.c <- unique(tr,by=c("client","siret"))[j=list(client,siret)]
        
        # On regroupe la table par client puis on test si le siret deu commerçant est présent dans au moins une des transactions du client
        c.group <- group_by(tr.c, client)
        autres <- filter(c.group, KEY$siret %in% siret)
        
        # On compte le nombre d'occurrences pour chaque client. S'il est supérieur à un, c'est un infidèle
        # fidelite[1] = TRUE : clients fidèles
        # fidelite[2] = FALSE : clients infidèles
        # fidelite[3] : prospects
        fidelite <<- c(table(summarise(autres, n=n())$n==1))
        fidelite[3] <<- nrow(unique(tr,by=c("client")))-(sum(fidelite))
        fidelite <<- cbind(melt(fidelite), type = c("Infidèles","Fidèles","Prospects"))
        fidelite
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
                div(tags$h2(ifelse(fidelite[fidelite$type=="Infidèles",1]>0,fidelite[fidelite$type=="Infidèles",1],0)), tags$h5("Clients infidèles")),
                div(tags$h2(ifelse(fidelite[fidelite$type=="Fidèles",1]>0,fidelite[fidelite$type=="Fidèles",1],0)), tags$h5("Clients fidèles"))
        )
      })
      
      ###################################################################################################################
      #                                          MODULE PROFIL DES CLIENTS                                              #
      ###################################################################################################################
      
      updateCritere <- function(){
        
        # Si la table est mauvaise on réinitialise les critères avec ceux ayant conduit à une bonne table (crit.panier)
        updateSelectInput(session, inputId = "sexe",
                          label = "Sexe",
                          choices = c(Choisir = '', "M", "F"),
                          selected = crit.panier$sexeClients)
        
        updateSelectInput(session, inputId = "csp",
                          label = "Catégorie socioprofessionnelle",
                          choices = c(Choisir = '', "AGRICULTEURS", "ARTISANTS COMMERCANTS ET CHEFS D'ENTREPRISES", "AUTRES SANS ACTIVITE PROF.",
                                      "CADRES ET PROF INTELLECTUELLES", "EMPLOYES", "OUVRIERS", "PROFESSIONS INTERMEDIAIRES", "RETRAITES"),
                          selected = crit.panier$cspClients)
        
        updateSelectInput(session, inputId = "situation",
                          label = "Situation familiale",
                          choices = c(Choisir = "", "CELIBATAIRE", "CONCUBIN", "DIVORCE", "INIT",
                                      "MARIE", "PACSE", "SEPARE", "VEUF"),
                          selected = crit.panier$situationClients)
      }
      
      panier <- reactive({
        
        # On récupère les critères nouveaux au fur et à mesure qu'ils sont entrés par l'utilisateur
        crit.new <- data.frame(dateClients = c(input$date.vis),
                               sexeClients = c(input$sexe),
                               cspClients = c(input$csp),
                               situationClients = c(input$situation))
        
        # On récupère dans une nouvelle table les transactions pour un tel jour et les montants positifs
        # Les montants négatifs correspondant à des retours ou autres on ne les considère pas
        df.new <- subset(df.a, date == input$date.vis & montant > 0)
        
        # On filtre les données
        if(input$sexe!="") {
          df.new <- subset(df.new, sexe == input$sexe)
        }
        
        if(input$csp!="")
          df.new <- subset(df.new, csp == input$csp)
        
        if(input$situation!="")
          df.new <- subset(df.new, situation == input$situation)
        
        # Enfin on filtre selon l'âge
        df.new <- subset(df.new, age >= input$age[1] & age <= input$age[2])
        getDataPanier(df.new, crit.new)
      })
      
      getDataPanier <- function(df, crit){
        
        if(nrow(df)>=2) {
          # ie que la nouvelle table créée est bonne donc on la stocke pour une utilisation ultérieure
          df.panier <<- df
          crit.panier <<- crit
          ggvisPanier()
        } else {
          # On update les sélecteurs
          updateCritere()
        }
      }
      
      # Graphique ggvis
      ggvisPanier <- function(){
        df.panier %>%
          ggvis(x = ~montant) %>%
          layer_densities(adjust = input$bin)        
      } %>%
        bind_shiny('panier-plot')
      
      # Nombre de lignes de la table filtrée
      output$nb <- renderUI({
        
        # Point d'entrée du module 'Panier de la clientèle'
        panier()
        tagList(tags$h4(paste("Visualisation sur", nrow(df.panier), "clients"))
        )
      })
      
      # Fonction forçant l'apparition de la fenêtre 'modal' sans trigger
      fenetreModal <- function(){
        toggleModal(session, "modAver")
      }
      
      output$modalUI <- renderUI({
        # On récupère en variable locales les critères de l'utilisateur
        # L'écrire en dur dans HTML(...) est buggé
        sexe <- input$sexe
        csp <- input$csp
        situation <- input$situation
        
        bsModal("modAver", "Profil des clients", trigger = "btnModal",
                tags$p("Ci-dessous le profil des clients ayant effectué une (ou plusieurs) transaction(s) le ",
                       tags$strong(crit.panier$dateClients), "et correspondant aux critères suivants :",
                       tags$ul(
                         tags$li(HTML(paste("de sexe", ifelse(sexe=="M"|sexe=="F", paste(tags$strong(sexe)), paste(tags$strong("masculin"), "ou", tags$strong("féminin")))))),
                         tags$li(HTML(paste("de CSP", ifelse(csp!="", paste(tags$strong(csp)), paste(tags$strong("quelconque")))))),
                         tags$li(HTML(paste("de situation", ifelse(situation!="", paste(tags$strong(situation)), paste(tags$strong("quelconque"))))))
                       )
                ),
                br(),
                tags$div(class = "row-fluid",
                         tags$div(class = "span12",
                                  dataTableOutput("transactionsClientsPrecis"))
                )
        )
      })
      
      # Table des transactions
      output$transactionsClientsPrecis <- renderDataTable({
        feinterReactivite()
        unique(subset(df.panier, select = c("age", "sexe", "csp", "situation", "villeclient")))
      })
      
      feinterReactivite <- reactive({
        date <- input$date.vis
        sexe <- input$sexe
        csp <- input$csp
        situation <- input$situation
        age <- input$age
      })
      
      # Astuce pour pouvoir réinitialiser les variables 'input'
      output$resetable_input <- renderUI({
        
        times <- input$reset_input
        div(id = letters[(times %% length(letters)) + 1],
            # Date de visualisation des données
            dateInput(inputId = "date.vis", label = "Date de visualisation du panier", value = "2013-05-05", language = "fr"),
            
            # Faire apparaître le choix du sexe
            selectInput("sexe",
                        label = "Sexe",
                        choices = c(Choisir = '', "M", "F"),
                        selectize = TRUE
            ),
            
            # Faire apparaître le choix de la csp
            selectInput("csp",
                        label = "Catégorie Socioprofessionnelle",
                        choices = c(Choisir = '', "AGRICULTEURS", "ARTISANTS COMMERCANTS ET CHEFS D'ENTREPRISES", "AUTRES SANS ACTIVITE PROF.",
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
      
      ###################################################################################################################
      #                                       FIN MODULE PROFIL DES CLIENTS                                             #
      ###################################################################################################################
      
      
      
      ###################################################################################################################
      #                                     OPTIMISER LE CALCUL DU BARYCENTRE                                           #
      ###################################################################################################################
      #       output$prospects <- renderDataTable({
      #         barycentre(recapitulatif(commercants.table))
      #         action(input$distance)
      #       })
      ###################################################################################################################
      #                                                                                                                 #
      ###################################################################################################################
    
    }
  })
})