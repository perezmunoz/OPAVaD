################
###### ui ######
################

require(shiny)
library(shinyBS)
library(ggvis)
require(leaflet)
require(ggplot2)
require(data.table)
require(bit64)
require(dplyr)
require(reshape2)

shinyUI(navbarPage("OPAVaD", id = "opavad",
                   tabPanel("Activité",
                            # Scripts de style et gérant l'apparition du loader ajax
                            tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
                                      tags$script(src="nav.js"),
                                      tags$script(type="text/javascript", src = "busy.js")
                            ),
                            # Image ajax loader
                            div(class = "busy", id = "loading",
                                p("Affichage en cours..."),
                                img(src="ajaxloader.gif")
                            ),
                            # Interface de connexion
                            div(class = "login",
                                uiOutput("uiLogin"),
                                textOutput("pass")
                            ),
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
                                                                          # Graphique 'Distribution du chiffre d'affaire selon la période de la journée'
                                                                          plotOutput("graphiqueCA"))
                                                                 ),
                                                                 fluidRow(
                                                                   column(12,
                                                                          # Texte expliquant le découpage de la journée
                                                                          uiOutput("moncommerceExplication")
                                                                   )
                                                                 )
                                                        ),
                                                        tabPanel("Fidélité de la clientèle", 
                                                                 column(2,
                                                                        # Répartitions des clients en terme de prospects, clients fidèles et clients infidèles
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
                                                        ),
                                                        tabPanel("Panier de la clientèle",
                                                                 column(9,
                                                                        ggvisOutput('panier-plot')
                                                                 ),
                                                                 column(3,
                                                                        # Fenêtre 'modal'
                                                                        uiOutput("modalUI"),
                                                                        
                                                                        # Ensemble des inputs encapsulés pour une réinitialisation ultérieure
                                                                        uiOutput('resetable_input'),
                                                                        
                                                                        # Réglage de la précision de la distribution
                                                                        sliderInput(inputId = "bin",
                                                                                    label = "Ajustement de la précision",
                                                                                    min = .1, max = 2, value = 1, step = .1),
                                                                        
                                                                        br(),
                                                                        
                                                                        # Bouton servan à réinitialiser les variables 'input'
                                                                        actionButton("reset_input", "Reset inputs"),
                                                                        
                                                                        br(),
                                                                        
                                                                        # Bouton affichant la table des transactions
                                                                        bsButton("btnModal", "Open Modal", style = "primary"),
                                                                        
                                                                        br(),
                                                                        uiOutput('nb'))
                                                        )
                                            )
                                     )
                            )
                   ),
                   # TO DO
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
                   # TO DO
                   tabPanel("A propos")
))