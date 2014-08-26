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
  
  # UI de connexion
  div(class = "login",
      uiOutput("uiLogin"),
      textOutput("pass")
  ),
  
  # UI carte
  div(class = "dashbord",
      uiOutput("uiMap"),
      uiOutput("uiMerchant")
  ),
  fluidRow(
    column(8, offset = 2,
           chartOutput('mapColombier', 'nvd3')
    )
  )
  #   plotOutput("irisPlot")
  #   chartOutput("irisPlot")
))