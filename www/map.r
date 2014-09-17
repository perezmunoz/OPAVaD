###########################
###### Map UI module ######
###########################

library(shiny)
library(leaflet)
library(rCharts)
library(maps)

# Carte centrée sur le commerçant connecté à la connexion
output$carte <- renderUI({
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
output$panneauCarte <- renderUI({
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
output$comparerByCritere <- renderUI({
  selectInput("compareMap", "Comparer par", c("Montants", "Transactions"))
})
output$comparerByPeriode <- renderUI({
  dateRangeInput("range", label = "P\u00E9riode de visualisation des donn\u00E9es", start = "2013-05-01", end = "2013-05-08", 
                 format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "fr", separator = " \u00E0 ")
})
output$separator <- renderUI({
  hr()
})
output$detailsComparaison <- renderUI({
  hr()
  tagList(tags$b("Carte"),
          tags$h6("Est affich\u00E9 sur la carte les commer\u00E7ants de m\u00EAme NAF que vous selon des cercles. Votre commerce (en bleu) peut \u00EAtre alors compar\u00E9 aux autres qui soient (vert) ou non affili\u00E9s (rouge) au Cr\u00E9dit Agricole."),
#           br(),
          tags$b("Type de comparaison"),
          tags$h6(HTML("Le graphique 'Distribution du chiffre d'affaire selon la p\u00E9riode de la journ\u00E9e' est visualisable par montants ou par nombre de transactions.
                  De la m\u00EAme mani\u00E8re, les cercles affich\u00E9s sur la carte sont proportionnels au type de comparaison")),
#           br(),
          tags$b("P\u00E9riode de visualisation"),
          tags$h6("Cercles et graphiques sont mis \u00E0 jour en fonction de la p\u00E9riode de visualisation."))
})

# à = \u00E0

# Description du commerçant
# output$desc <- renderUI({
#   print("[output$desc]")
#   as.character(tagList(
#     tags$h2('Tableau de bord')
#     tags$p('La carte ci-dessus est centr\u00E9e sur votre fond de commerce immatricul\u00E9 sous le num\u00E9ro de SIRET ', KEY$siret),
#     tags$p('situ\u00E9  la latitude, longitude : ', KEY$latitude, ',', KEY$longitude, '.')
#   ))
# })

# # Séparateur description commerçant - graphique n°1
# output$separator <- renderUI({
#   hr()
# })