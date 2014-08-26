################
###### ui ######
################

library(shiny)
library(leaflet)
library(rCharts)
library(ggplot2)

shinyUI(fluidPage(
  
  tags$head(tags$link(rel='stylesheet', type='text/css', href='styles.css')
  ),
  # Interface de connexion
  div(class = "login",
      uiOutput("uiLogin"),
      textOutput("pass")
  ),
  # Mon fond de commerce
  ## Carte et panneau 'Mon fond de commerce'
  fluidRow(
    uiOutput("uiMap"),
    uiOutput("uiMerchant")
  ),
  # Détails sur le commerçant
  fluidRow(
    column(6,
           offset = 3,
           uiOutput("desc")
    )
  ),
  # Séparateur description du commerçant et graphiques
  fluidRow(uiOutput("separator")
  ),
  # Graphique n°1
  fluidRow(
    column(6,
           offset = 3,
           plotOutput("ca")
    )
  )
))