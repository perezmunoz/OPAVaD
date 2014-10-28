################
###### ui ######
################

library(shiny)
library(shinyBS)
library(ggvis)
library(leaflet)
library(ggplot2)
library(data.table)
library(bit64)
library(plyr)
library(dplyr)
library(reshape2)

shinyUI(navbarPage("OPAVaD", id = "opavad",
                   tabPanel("Activité",
                            # Scripts de style et gérant l'apparition du loader ajax
                            tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
                                      tags$link(rel="stylesheet", href="//maxcdn.bootstrapcdn.com/font-awesome/4.2.0/css/font-awesome.min.css"),
                                      tags$script(src="nav.js"),
                                      tags$script(type="text/javascript", src = "busy.js"),
                                      # Script permettant d'envoyer directement du code JavaScript depuis R au client
                                      tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",
                                                          function(message) {
                                                            eval(message.value);
                                                          }
                                                          );
                                                        '))
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
                                     # Outils de comparaison et explication de l'application
                                     conditionalPanel(condition = "$($('#graphique-tab').children()[0]).hasClass('active')
                                                                  || $($('#graphique-tab').children()[1]).hasClass('active')",
                                                      column(2,
                                                             offset = 2,
                                                             uiOutput("comparerByCritere"),
                                                             uiOutput("comparerByPeriode"),
                                                             uiOutput("separator"),
                                                             uiOutput("detailsComparaison")
                                                      )
                                     ),
                                     conditionalPanel(condition = "$($('#graphique-tab').children()[2]).hasClass('active')",
                                                      column(2,
                                                             offset = 2,
                                                             # Fenêtre 'modal' pour le panier des clients dans 'Fidélité de la clientèle'
                                                             uiOutput("modalUIPanier"),
                                                             # Ensemble des inputs encapsulés pour une réinitialisation ultérieure
                                                             uiOutput('resetable_input'),
                                                             # Composants pour le réglage de la visualisation du graphique
                                                             uiOutput('composantsReglagePanier')
                                                      )
                                     ),
                                     column(6,
                                            tabsetPanel(id = "graphique-tab",
                                                        tabPanel("Mon commerce", id = "moncommerce",
                                                                 fluidRow(id = "idcaTitre",
                                                                          column(12,
                                                                                 # Titre du graphique 'Répartition du chiffre d'affaire'
                                                                                 uiOutput("caTitre")
                                                                          )
                                                                 ),
                                                                 fluidRow(
                                                                   column(12,
                                                                          # Message d'erreur en cas de graphique nul
                                                                          uiOutput("errorMessageCA")
                                                                   )
                                                                 ),
                                                                 fluidRow(
                                                                   column(12,
                                                                          # Graphique 'Distribution du chiffre d'affaire selon la période de la journée'
                                                                          plotOutput("graphiqueCA")
                                                                   )
                                                                 ),
                                                                 fluidRow(
                                                                   column(12,
                                                                          # Texte expliquant le découpage de la journée
                                                                          uiOutput("moncommerceExplication")
                                                                   )
                                                                 )
                                                        ),
                                                        tabPanel("Fidélité de la clientèle",
                                                                 fluidRow(
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
                                                                 # Séparateur début description section sur la répartion des clients 
                                                                 hr(),
                                                                 fluidRow(id = "descriptionRepartitionClientele",
                                                                          # Fenêtre 'modal' pour le profil des clients
                                                                          uiOutput("modalUIProfil"),
                                                                          # Texte introductif des graphiques sur la répartition
                                                                          uiOutput("repartitionClienteleCategories"),
                                                                          # Bouton mdal permettant de voir les profils de clients qui consomment le plus chez ce commerçant
                                                                          bsButton("btnModalProfil", "Profils des clients", style = "primary")
                                                                 ),
                                                                 hr(),
                                                                 # Séparateur fin description section sur la répartion des clients 
                                                                 fluidRow(
                                                                   column(6, 
                                                                          id = "idclienteleSexeTitre",
                                                                          # Titre du graphique 'Répartition de la clientèle selon le sexe'
                                                                          uiOutput("clienteleSexeTitre"),
                                                                          # Graphique donnant la répartition selon le sexe des clients fidèles et infidèles
                                                                          plotOutput("graphiqueClienteleFidInfSexe")
                                                                   ),
                                                                   column(6,
                                                                          id = "idclienteleAgeTitre",
                                                                          # Titre du graphique 'Répartition de la clientèle selon l'âge'
                                                                          uiOutput("clienteleAgeTitre"),
                                                                          # Graphique donnant la répartition selon l'âge des clients fidèles et infidèles
                                                                          plotOutput("graphiqueClienteleFidInfAge")
                                                                   )
                                                                 ),
                                                                 fluidRow(
                                                                   column(6,
                                                                          id = "idclienteleCSPTitre",
                                                                          # Titre du graphique 'Répartition de la clientèle selon la csp'
                                                                          uiOutput("clienteleCSPTitre"),
                                                                          # Graphique donnant la répartition selon la csp des clients fidèles et infidèles
                                                                          plotOutput("graphiqueClienteleFidInfCSP")
                                                                   ),
                                                                   column(6,
                                                                          id = "idclienteleSituationTitre",
                                                                          # Titre du graphique 'Répartition de la clientèle selon la situation'
                                                                          uiOutput("clienteleSituationTitre"),
                                                                          # Graphique donnant la répartition selon la situation des clients fidèles et infidèles
                                                                          plotOutput("graphiqueClienteleFidInfSituation")
                                                                   )
                                                                 )
                                                        ),
                                                        tabPanel("Panier de la clientèle",
                                                                 fluidRow(
                                                                   column(12,
                                                                          fluidRow(id = "idpanierTitre",
                                                                                   h4("Distribution journalière du panier des clients selon des critères"),
                                                                                   # Titre du graphique 'Distribution du panier journalier'
                                                                                   uiOutput("panierSousTitre"),
                                                                                   uiOutput("errorPanierTitre"),
                                                                                   dataTableOutput("tableTransctionsPanierSansGraphique")
                                                                          ),
                                                                          fluidRow(
                                                                            # Graphique 'Distribution du panier journalier'
                                                                            #                                                                             ggvisOutput("graphiquePanier")
                                                                            plotOutput("graphiquePanier")
                                                                          )
                                                                   )
                                                                 )
                                                        )
                                            )
                                     )
                            )
                   ),
                   tabPanel("Prospection",
                            # Image ajax loader
                            div(class = "busy", id = "loading",
                                p("Affichage en cours..."),
                                img(src = "ajaxloader.gif")
                            ),
                            br(),
                            fluidRow(
                              # Colonne principale, contenant toutes les informations et éléments pour prospecter
                              column(6,
                                     offset = 2,
                                     # Première ligne de choix pour la prospection
                                     fluidRow(
                                       # Choix du sexe
                                       column(6,
                                              fluidRow(
                                                column(5,
                                                       offset = 1,
                                                       img(src = "boygirl.jpeg", id = "img-sexe", width = "120px", height = "120px")
                                                ),
                                                column(5,
                                                       checkboxGroupInput("checkBoxProspectionSexe", label = h4("Sexe"), 
                                                                          choices = list("Homme" = "M", "Femme" = "F"),
                                                                          selected = "M")
                                                )
                                              )
                                       ),
                                       # Choix de l'âge
                                       column(6,
                                              fluidRow(
                                                img(src = "age.png", id = "img-age", width = "120px", height = "120px")
                                              ),
                                              fluidRow(
                                                column(3,
                                                       h4("Age")
                                                ),
                                                column(9,
                                                       sliderInput("selectInputProspectionAge", label = "", min = 0, 
                                                                   max = 100, value = c(18, 25))
                                                )
                                              )
                                       )
                                     ),
                                     br(),
                                     br(),
                                     # Ligne contennat les images pour la situation familiale et la CSP
                                     fluidRow(
                                       # Colonne avec l'image de la situation familiale
                                       column(6,
                                              img(src = "situation.gif", id = "img-situation", width = "120px", height = "120px")
                                       ),
                                       # Colonne avec l'image de la CSP
                                       column(6,
                                              img(src = "engrenage.png", id = "img-csp", width = "120px", height = "120px")
                                       )
                                     ),
                                     # Ligne contenant les sélecteurs pour la situation familiale et la CSP
                                     fluidRow(
                                       # Sélecteur de la situation familiale
                                       column(6,
                                              column(3,
                                                     h4("Situation")
                                              ),
                                              column(9,
                                                     selectInput("selectInputSituationProspection",
                                                                 label = "",
                                                                 choices = c(Choisir = "", "CELIBATAIRE", "CONCUBIN", "DIVORCE", "INIT",
                                                                             "MARIE", "PACSE", "SEPARE", "VEUF"),
                                                                 multiple = TRUE, selectize = TRUE)
                                              )
                                       ),
                                       # Sélecteur de la CSP
                                       column(6,
                                              column(3,
                                                     h4("CSP")
                                              ),
                                              column(9,
                                                     selectInput("selectInputCSPProspection",
                                                                 label = "",
                                                                 choices = c(Choisir = '', "AGRICULTEURS", "ARTISANTS COMMERCANTS ET CHEFS D'ENTREPRISES", "AUTRES SANS ACTIVITE PROF.",
                                                                             "CADRES ET PROF INTELLECTUELLES", "EMPLOYES", "OUVRIERS", "PROFESSIONS INTERMEDIAIRES", "RETRAITES"),
                                                                 multiple = TRUE, selectize = TRUE)
                                              )
                                       )
                                     ),
                                     hr(),
                                     fluidRow(
                                       column(6,
                                              h4("Critères", style="text-align:center;"),
                                              uiOutput("criteresProspectionDesc")
                                       ),
                                       column(6,
                                              numericInput(inputId = "numericInputChampAction", label = h4("Dans un périmètre de (km)"), value = 5, min = 0, step = 1),
                                              # Pour mémo :
                                              # fid = 1 : clients fidèles
                                              # fid = 2 : clients infidèles
                                              checkboxGroupInput("checkBoxFideliteProspection", label = h4("Considérer a priori les clients fidèles/infidèles comme des prospects ?"), 
                                                                 choices = list("Clients fidèles" = "1", "Clients infidèles" = "2"),
                                                                 selected = c("1", "2"))
                                       )
                                     ),
                                     hr(),
                                     fluidRow(
                                       h4("Lancer la prospection", style="text-align:center;"),
                                       actionButton(inputId = "actionButtonLancerProspection", label = "", icon = icon("pie-chart", "fa-3x", lib = "font-awesome"))
                                     )
                              ),
                              column(2,
                                     # Résultat de la prospection
                                     uiOutput("prospectionObjectif"),
                                     # Séparateur
                                     hr(),
                                     # Chiffres récapitulatifs
                                     uiOutput("prospectionRecapitulatifClientele")
                              )
                            )
                            
                   )
))