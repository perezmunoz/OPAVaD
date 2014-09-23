################
###### ui ######
################

library(shiny)
library(leaflet)
library(ggplot2)
library(data.table)

shinyUI(navbarPage("OPAVaD", id = "opavad",
                   tabPanel("Activité",
                            tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
                                      tags$script(src="nav.js"),
                                      tags$script(type="text/javascript", src = "busy.js")
                            ),
                            # Ajax loader
                            div(class = "busy", id = "loading",
                                p("Affichage en cours..."),
                                img(src="ajaxloader.gif")
                            ),
                            # Interface de connexion
                            div(class = "login",
                                uiOutput("uiLogin"),
                                textOutput("pass")
                            ),
                            # Mon fond de commerce
                            # Carte et panneau 'Mon commerce'
                            fluidRow(
                              uiOutput("carte"),
                              uiOutput("panneauCarte")
                            ),
                            fluidRow(id = "data-viz",
                                     column(2,
                                            offset = 2,
                                            uiOutput("comparerByCritere"),
                                            uiOutput("comparerByPeriode"),
                                            uiOutput("separator"),
                                            uiOutput("detailsComparaison")
                                     ),
                                     column(6,
                                            tabsetPanel(id = "graphique-tab",
                                                        tabPanel("Mon commerce",
                                                                 fluidRow(id = "idcaTitre",
                                                                          column(12,
                                                                                 # Titre du graphique 'Répartition du chiffre d'affaire'
                                                                                 uiOutput("caTitre")
                                                                          )
                                                                 ),
                                                                 fluidRow(
                                                                   column(12,
                                                                          # Graphique 'Répartition du chiffre d'affaire'
                                                                          plotOutput("graphiqueCA"))
                                                                 ),
                                                                 fluidRow(
                                                                   column(12,
                                                                          # Texte expliquant le découpage de la journée
                                                                          uiOutput("moncommerceExplication")
                                                                   )
                                                                 )
                                                        ),
                                                        tabPanel("Fidélité clients", 
                                                                 column(2,
                                                                        # Indications sur le nombrs de prospects, clients fidèles et clienst infidèles
                                                                        uiOutput("repartitionClientele")
                                                                 ),
                                                                 column(6,
                                                                        fluidRow(id = "idclienteleTitre",
                                                                                 # Titre du graphique 'Répartition de la clientèle'
                                                                                 uiOutput("clienteleTitre")
                                                                        ),
                                                                        fluidRow(
                                                                          # Graphique 'Répartition de la clientèle'
                                                                          plotOutput("graphiqueClientele")
                                                                        )
                                                                 ),
                                                                 column(4,
                                                                        # Définition des notions utilisées
                                                                        uiOutput("clienteleDefinition")
                                                                 )
                                                        )
                                            )
                                     )
                            )
                   ),
                   tabPanel("Prospection",
                            div(class = "busy", id = "loading",
                                p("Affichage en cours..."),
                                img(src="ajaxloader.gif")
                            ),
                            fluidRow(
                              # Entré du score minimum
                              numericInput("distance", label = h3("Champ d'action de la campagne (en km)"), min=0, value=10)
                            ),
                            hr(),
                            fluidRow(
                              dataTableOutput("prospects")
                            )
                   ),
                   tabPanel("A propos")
))