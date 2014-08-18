###########################
###### Map UI module ######
###########################

library(shiny)
library(leaflet)
library(rCharts)
library(maps)
library(ggplot2)

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

# Panneau du fond de commerce du commerçant
output$uiMerchant <- renderUI({
  absolutePanel(id = "dashbord", class = "modal", fixed = TRUE, draggable = FALSE,
                top = 20, left = "auto", right = 20, bottom = "auto",
                width = "auto", height = "30%",
                
                as.character(tagList(
                  tags$h5("Mon fond de commerce"),
                  tags$strong(KEY$rs), tags$br(),
                  tags$strong("Siret : "), sprintf("%s", KEY$siret), tags$br(),
                  tags$strong("NAF : "), sprintf("%s", KEY$naf), tags$br(),
                  #tags$strong("Groupe NAF : "), sprintf("%s", KEY$groupe_naf), tags$br(),
                  tags$strong("Total :"), sprintf("%s €", KEY$montant), tags$br(),
                  tags$strong("Transactions : "), sprintf("%s", KEY$transaction), tags$br(),
                  tags$br()
                )),
                
                selectInput("compareMap", "Compare by", c("Montants", "Transactions"))
  )
})

output$mapColombier <- renderChart ({
  colombier <- nPlot(y = MAPcolombier.data$Montant, x = MAPcolombier.data$date_transaction, group = "period" , data = MAPcolombier.data, 
                     type = 'multiBarChart')
  colombier$set(dom = "mapColombier")
  return(colombier)
})

# output$irisPlot <- renderPlot({
#   data(iris)
#   str(iris)
#   p <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) + geom_point()
#   print(p)
# })
