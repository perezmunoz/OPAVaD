###########################
###### Map UI module ######
###########################

library(shiny)
library(leaflet)
library(rCharts)
library(maps)

# Carte centrée sur le commerçant connecté à la connexion
output$uiMap <- renderUI({
  print("[output$uiMap]")
  leafletMap(
    "map", "100%", "40%",
    initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
    options = list(
      zoom = 16,
      center = c(KEY$latitude, KEY$longitude),
      maxBounds = list(list(47.408167, -2.644132), list(48.836306, -0.743497))
    )
  )
})

# Panneau 'Mon fond de commerce'
output$uiMerchant <- renderUI({
  print("[output$uiMerchant]")
  data <- getData()
  com <<- data[data$siret == KEY$siret, ]
  print(com)
  absolutePanel(id = "dashbord", class = "modal", fixed = TRUE, draggable = FALSE,
                top = 57, left = 330, right = "auto", bottom = "auto",
                width = "auto", height = "auto",
                
                as.character(tagList(
                  tags$h3(com$rs), tags$br(),
                  tags$strong("Siret : "), sprintf("%s", com$siret), tags$br(),
                  tags$strong("NAF : "), sprintf("%s", com$naf), tags$br(),
                  tags$strong("Chiffre d'affaires :"), sprintf("%s", com$montant), HTML("\u20AC"), tags$br(),
                  tags$strong("Transactions : "), sprintf("%s", com$transaction), tags$br(),
                  tags$strong("Latitude : "), sprintf("%s", com$latitude), tags$br(),
                  tags$strong("Longitude : "), sprintf("%s", com$longitude), tags$br()
                ))
  )
})

# Panneau de comparaison entre commerçants
output$comparerCritere <- renderUI({
  selectInput("compareMap", "Comparer par", c("Montants", "Transactions"))
})

output$comparerPeriode <- renderUI({
  dateRangeInput("range", label = "P\u00E9riode de visualisation des donn\u00E9es", start = "2013-05-01", end = "2013-05-20", 
                 format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "fr", separator = " \u00E0 ")  
})

# à = \u00E0

# Description du commerçant
output$desc <- renderUI({
  print("[output$desc]")
  as.character(tagList(
#     tags$h2('Tableau de bord')
#     tags$p('La carte ci-dessus est centr\u00E9e sur votre fond de commerce immatricul\u00E9 sous le num\u00E9ro de SIRET ', KEY$siret),
#     tags$p('situ\u00E9  la latitude, longitude : ', KEY$latitude, ',', KEY$longitude, '.')
  ))
})

# # Séparateur description commerçant - graphique n°1
# output$separator <- renderUI({
#   hr()
# })