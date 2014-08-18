####################
###### server ######
####################

library(shiny)
library(leaflet)
library(rCharts)
library(maps)
library(ggplot2)

Logged = FALSE;

#From a future version of Shiny
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
      
#       output$mapColombier <- renderChart ({
#         colombier <- nPlot(y = MAPcolombier.data$Montant, x = MAPcolombier.data$date_transaction, group = "period" , data = MAPcolombier.data, 
#                            type = 'multiBarChart')
#         colombier$set(dom = "mapColombier")
#         return(colombier)
#       })
      
      source('www/map.r', local = TRUE)
      
      compareCol <- reactive({
        if (input$compareMap == "Montants") {
          return("montant")
        } else {return("transaction")}
      })
      
      group_naf <- commercants[commercants$naf == KEY$naf, ]
      
      makeReactiveBinding('selectedCom')
      
      #Commerçants présents dans la champ de vision
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
      # Fonction différenciant les commerçants affiliés et non affiliés
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
    }
  })
})