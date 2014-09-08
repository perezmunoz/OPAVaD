################
###### ui ######
################

library(shiny)
library(leaflet)
library(rCharts)
library(ggplot2)

shinyUI(navbarPage("OPAVaD", id = "opavad",
                   tabPanel("Activité",
                            tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
                                      tags$script(src="nav.js")
                            ),
                            # Interface de connexion
                            div(class = "login",
                                uiOutput("uiLogin"),
                                textOutput("pass")
                            ),
                            # Mon fond de commerce
                            # Carte et panneau 'Mon fond de commerce'
                            fluidRow(
                              uiOutput("uiMap"),
                              uiOutput("uiMerchant")
                            ),
                            #                             # Détails sur le commerçant
                            #                             fluidRow(
                            #                               #                               column(3,
                            #                               #                                      fluidRow(uiOutput("comparerCritere")),
                            #                               #                                      fluidRow(uiOutput("comparerPeriode"))
                            #                               #                               ),
                            #                               column(6,
                            #                                      offset = 3,
                            #                                      #fluidRow(uiOutput("desc")),
                            #                                      fluidRow(
                            #                                        column(6,
                            #                                               uiOutput("comparerCritere")
                            #                                        ),
                            #                                        column(6,
                            #                                               uiOutput("comparerPeriode")
                            #                                        )
                            #                                      )
                            #                               )
                            #                             ),
                            # #                             # Séparateur description du commerçant et graphiques
                            # #                             fluidRow(uiOutput("separator")
                            # #                             ),
                            #                             # Graphique n°1
                            #                             fluidRow(
                            #                               column(6,
                            #                                      offset = 3,
                            #                                      tabsetPanel(
                            #                                        tabPanel("CA", plotOutput("ca")),
                            #                                        tabPanel("Fidélité")
                            #                                      )
                            #                               )
                            #                             )
                            fluidRow(
                              column(2,
                                     offset = 2,
                                     uiOutput("comparerCritere"),
                                     uiOutput("comparerPeriode")
                                     ),
                              column(6,
                                     tabsetPanel(id = "graphique-tab",
                                       tabPanel("Mon commerce", plotOutput("ca")),
                                       tabPanel("Clientèle")
                                       )
                                     )
                              )
                   ),
                   tabPanel("Prospection")
))