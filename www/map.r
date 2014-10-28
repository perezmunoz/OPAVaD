###########################
###### Map UI module ######
###########################

# Carte centrée sur le commerçant connecté à la connexion
output$carte <- renderUI({
  leafletMap(
    "map", "100%", 400,
    initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
    options = list(
      zoom = 16,
      center = c(KEY$lat, KEY$lon),
      maxBounds = list(list(47.408167, -2.644132), list(48.836306, -0.743497))
    )
  )
})

# Panneau 'Mon activité'
output$panneauCarte <- renderUI({
  absolutePanel(id = "dashbord", class = "modal", fixed = FALSE, draggable = FALSE,
                top = 57, left = 330, right = "auto", bottom = "auto",
                width = "auto", height = "auto",
                
                as.character(tagList(
                  tags$h3(KEY$rs),
                  tags$strong("Siret : "), sprintf("%s", KEY$siret), tags$br(),
                  tags$strong("NAF : "), sprintf("%s", KEY$naf), tags$br(),
                  tags$strong("Latitude : "), sprintf("%s", KEY$lat), tags$br(),
                  tags$strong("Longitude : "), sprintf("%s", KEY$lon), tags$br()
                ))
  )
})

# Panneau de comparaison entre commerçants par type
output$comparerByCritere <- renderUI({
  selectInput("compareMap", "Comparer par", c("Montants", "Transactions"))
})

# Panneau de comparaison entre commerçants par date
output$comparerByPeriode <- renderUI({
  dateRangeInput("range", label = "P\u00E9riode de visualisation des donn\u00E9es", start = "2013-05-01", end = "2013-05-08", min = "2012-09-28", max = "2013-10-02",
                 format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "fr", separator = " \u00E0 ")
})

# Séparateur entre les éléments de comparaison et le texte explicatif
output$separator <- renderUI({
  hr()
})

# Text explicatif des divers éléments affichés
output$detailsComparaison <- renderUI({
  hr()
  tagList(tags$b("Carte"),
          tags$h6("Est affich\u00E9 sur la carte les commer\u00E7ants de m\u00EAme NAF que vous selon des cercles. Votre commerce (en bleu) peut \u00EAtre alors compar\u00E9 aux autres qui soient (vert) ou non affili\u00E9s (rouge) au Cr\u00E9dit Agricole."),
          tags$b("Type de comparaison"),
          tags$h6("Le graphique 'Distribution du chiffre d'affaire selon la p\u00E9riode de la journ\u00E9e' est visualisable par montants ou par nombre de transactions.
                  De la m\u00EAme mani\u00E8re, les cercles affich\u00E9s sur la carte sont proportionnels au type de comparaison"),
          tags$b("P\u00E9riode de visualisation"),
          tags$h6("Cercles et graphiques sont mis \u00E0 jour en fonction de la p\u00E9riode de visualisation."))
})