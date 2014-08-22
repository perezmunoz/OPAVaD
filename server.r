####################
###### server ######
####################

library(shiny)
library(leaflet)
library(rCharts)
library(maps)
library(ggplot2)

Logged = FALSE;

# From a future version of Shiny
bindEvent <- function(eventExpr, callback, env=parent.frame(), quoted=FALSE) {
  eventFunc <- exprToFunction(eventExpr, env, quoted)
  
  initialized <- FALSE
  invisible(observe({
    eventVal <- eventFunc()
    if (!initialized)
      initialized <<- TRUE
    else
      isolate(callback())
  }))
}

shinyServer(function(input, output, session) {
  
  source('www/login.r',  local = TRUE)
  
  observe({
    if (USER$Logged == TRUE) {
      
      source('www/map.r', local = TRUE)
      
      # Merchant transactions loaded
      key.df.transactions <<- read.delim(getName(), sep = "\t", header = FALSE, dec = ".", col.names = varsTransactions)
      
      compareCol <- reactive({
        if (input$compareMap == "Montants") {
          return("montant")
        } else {return("transaction")}
      })
      
      group_naf <- commercants[commercants$naf == KEY$naf, ]
      
      makeReactiveBinding('selectedCom')
      
      # Merchants in bounds
      comInBounds <- reactive({
        if (is.null(input$map_bounds))
          return(group_naf[FALSE,])
        bounds <- input$map_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        inBounds <- subset(group_naf,
                           latitude >= latRng[1] & latitude <= latRng[2] &
                             longitude >= lngRng[1] & longitude <= lngRng[2])
        inBounds[order(inBounds[[compareCol()]], decreasing = TRUE), ]
      })
      
      # Create the map; this is not the "real" map, but rather a proxy
      # object that lets us control the leaflet map on the page.
      map <- createLeafletMap(session, 'map')
      
      observe({
        if (is.null(input$map_click))
          return()
        selectedCom <<- NULL
      })
      
      
      observe({
        map$clearShapes()
        
        listeCommercants <<- comInBounds()
        
        if (nrow(listeCommercants) == 0)
          return()
        
        map$addCircle(
          listeCommercants$latitude,
          listeCommercants$longitude,
          4 * sqrt(listeCommercants[[compareCol()]]) / input$map_zoom,
          row.names(listeCommercants),
          list(
            weight = 1.2,
            fill = TRUE,
            color = getColor(listeCommercants))
        )
      })
      # Color affiliate and non affiliates merchants
      getColor <- function(input) {
        
        ids <- c(row.names(input))
        category <- vector("list", nrow(input))
        for(i in 1:nrow(input)) {
          if (input[row.names(input) == ids[i], ]$affilie_ca == 'O') {
            if (row.names(KEY) == ids[i]) {
              category[i] <- '#00F'
            } else { category[i] <- '#4A9' }
          } else { category[i] <- '#F03' }    
        }
        return(category)
      }
      
      observe({
        event <- input$map_shape_click
        cat(event$id)
        if (is.null(event))
          return()
        map$clearPopups()
        
        isolate({
          listeCommercants <- comInBounds()
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
      # Plot sales merchant
      output$ca <- renderPlot ({
        
        # Structuring merchant's data
        key.df.transactions$date_transaction <- as.Date(key.df.transactions$date_transaction, format="%d/%m/%Y")
        print(str(key.df.transactions))
        key.df.transactions$numero_siret <- as.character(key.df.transactions$numero_siret)
        key.df.transactions$heure_transaction <- strptime(key.df.transactions$heure_transaction, "%H:%M:%S")
        
        period = (input$range[2] - input$range[1]) + 1
        print(period)
        #         selected <- df[df$numero_siret == '44031091000029', ]
        print(input$range[1])
        print(input$range[2])
        selected <- key.df.transactions[key.df.transactions$date_transaction >= input$range[1]
                                        & key.df.transactions$date_transaction <= input$range[2], ]
        selected <- selected[order(selected$date_transaction), ]
        
        matin.data <- selected[selected$heure_transaction >= strptime("08:00:00", "%H:%M:%S")
                               & selected$heure_transaction <= strptime("12:00:00", "%H:%M:%S"), ]
        matin.data$heure_transaction <- NULL
        matin.summary <- matin.data %.%
          group_by(date_transaction) %.%
          summarise(Montant = sum(montant_transaction),
                    Transactions = n())
        matin <- rep(matin, times = period)
        matin.summary$period <- matin
        
        midi.data <- selected[selected$heure_transaction > strptime("12:00:00", "%H:%M:%S")
                              & selected$heure_transaction <= strptime("14:00:00", "%H:%M:%S"), ]
        midi.data$heure_transaction <- NULL
        midi.summary <- midi.data %.%
          group_by(date_transaction) %.%
          summarise(Montant = sum(montant_transaction),
                    Transactions = n())
        midi <- rep(midi, times = period)
        midi.summary$period <- midi
        
        pm.data <- selected[selected$heure_transaction > strptime("14:00:00", "%H:%M:%S")
                            & selected$heure_transaction <= strptime("17:00:00", "%H:%M:%S"), ]
        pm.data$heure_transaction <- NULL
        pm.summary <- pm.data %.%
          group_by(date_transaction) %.%
          summarise(Montant = sum(montant_transaction),
                    Transactions = n())
        pm <- rep(pm, times = period)
        pm.summary$period <- pm
        
        soir.data <- selected[(selected$heure_transaction > strptime("17:00:00", "%H:%M:%S")
                               & selected$heure_transaction < strptime("23:59:59", "%H:%M:%S"))
                              | (selected$heure_transaction >= strptime("00:00:00", "%H:%M:%S")
                                 & selected$heure_transaction < strptime("08:00:00", "%H:%M:%S")), ]
        soir.data$heure_transaction <- NULL
        soir.summary <- soir.data %.%
          group_by(date_transaction) %.%
          summarise(Montant = sum(montant_transaction),
                    Transactions = n())
        soir <- rep(soir, times = period)
        soir.summary$period <- soir
        
        sel <- rbind(matin.summary, midi.summary)
        sel <- rbind(sel, pm.summary)
        fin <- rbind(sel, soir.summary)
        
        ggplot(data = fin, aes(x = date_transaction, y = Montant)) + geom_bar(stat = 'identity', aes(fill = period)) + theme_bw() + 
          theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank(), rect=element_blank())
      })     
    }
  })
})