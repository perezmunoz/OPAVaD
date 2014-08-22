################
###### ui ######
################

library(shiny)
library(leaflet)
library(rCharts)
library(ggplot2)

shinyUI(fluidPage(
  
  tags$head(tags$link(rel='stylesheet', type='text/css', href='styles.css'),
            tags$script(type="text/javascript", src = "busy.js")
  ),
  # Log in GUI
  div(class = "login",
      uiOutput("uiLogin"),
      textOutput("pass")
      #img(id = "loading")
      #src = "ajaxloader.gif", 
  ),
  # Map
  fluidRow(
    uiOutput("uiMap"),
    uiOutput("uiMerchant")
  ),
  # Merchant description
  fluidRow(
    column(6,
           offset = 3,
           uiOutput("desc")
    )
  ),
  hr(),
  fluidRow(
    column(6,
           offset = 3,
           plotOutput("ca")
    )
  )
))