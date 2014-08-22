###########################
###### Map UI module ######
###########################

library(shiny)
library(leaflet)
library(rCharts)
library(maps)

output$uiMap <- renderUI({
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

output$uiMerchant <- renderUI({
  absolutePanel(id = "dashbord", class = "modal", fixed = TRUE, draggable = FALSE,
                top = 20, left = 330, right = "auto", bottom = "auto",
                width = "auto", height = "30%",
                
                as.character(tagList(
                  tags$h5("Mon fond de commerce"),
                  tags$strong(KEY$rs), tags$br(),
                  tags$strong("Siret : "), sprintf("%s", KEY$siret), tags$br(),
                  tags$strong("NAF : "), sprintf("%s", KEY$naf), tags$br(),
                  tags$strong("Total :"), sprintf("%s €", KEY$montant), tags$br(),
                  tags$strong("Transactions : "), sprintf("%s", KEY$transaction), tags$br(),
                  tags$br()
                )),
                
                selectInput("compareMap", "Comparer par", c("Montants", "Transactions")),
                dateRangeInput("range", label = "P\u00E9riode de visualisation", start = "2013-05-01", end = "2013-05-20", 
                               format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "fr", separator = " \u00E0 ")
  )
})

# Link to unicode characters list
output$desc <- renderUI({
  as.character(tagList(
    tags$h2('Statistiques de mon fond de commerce'),
    tags$p('La carte ci-dessus est centr\u00E9e sur votre fond de commerce immatricul\u00E9 sous le num\u00E9ro de SIRET ', KEY$siret),
    tags$p('situ\u00E9 \u00E0 la latitude, longitude : ', KEY$latitude, ',', KEY$longitude, '.')
  ))
})