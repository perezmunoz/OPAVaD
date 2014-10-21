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
                                                                   column(9,
                                                                          fluidRow(id = "idpanierTitre",
                                                                                   # Titre du graphique 'Distribution du panier journalier'
                                                                                   uiOutput("panierTitre")
                                                                          ),
                                                                          fluidRow(
                                                                            # Graphique 'Distribution du panier journalier'
                                                                            ggvisOutput("graphiquePanier")
                                                                          )
                                                                   ),
                                                                   column(3,
                                                                          # Fenêtre 'modal' pour le panier des clients dans 'Fidélité de la clientèle'
                                                                          uiOutput("modalUIPanier"),
                                                                          
                                                                          # Ensemble des inputs encapsulés pour une réinitialisation ultérieure
                                                                          uiOutput('resetable_input'),
                                                                          
                                                                          # Réglage de la précision de la distribution
                                                                          sliderInput(inputId = "bin",
                                                                                      label = "Ajustement de la précision",
                                                                                      min = .1, max = 2, value = 1, step = .1),
                                                                          
                                                                          hr(),
                                                                          
                                                                          # Bouton affichant la table des transactions
                                                                          bsButton("btnModalPanier", "Voir données", style = "primary"),
                                                                          
                                                                          br(),
                                                                          
                                                                          # Bouton servan à réinitialiser les variables 'input'
                                                                          actionButton("reset_input", "Réinitialiser les critères")
                                                                   )
                                                                 ),
                                                                 fluidRow(
                                                                   uiOutput('nb')
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
                                                       checkboxGroupInput("btnProspectionSexe", label = h3("Sexe"), 
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
                                                       uiOutput("ageSelectInputProspectionTitre")
                                                ),
                                                column(9,
                                                       sliderInput("btnProspectionAge", label = "", min = 0, 
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
                                                     uiOutput("situationSelectInputProspectionTitre")
                                              ),
                                              column(9,
                                                     selectInput("btnSituationProspection",
                                                                 label = "",
                                                                 choices = c(Choisir = "", "CELIBATAIRE", "CONCUBIN", "DIVORCE", "INIT",
                                                                             "MARIE", "PACSE", "SEPARE", "VEUF"),
                                                                 multiple = TRUE, selectize = TRUE)
                                              )
                                       ),
                                       # Sélecteur de la CSP
                                       column(6,
                                              column(3,
                                                     uiOutput("CSPSelectInputProspectionTitre")      
                                              ),
                                              column(9,
                                                     selectInput("btnCSPProspection",
                                                                 label = "",
                                                                 choices = c(Choisir = '', "AGRICULTEURS", "ARTISANTS COMMERCANTS ET CHEFS D'ENTREPRISES", "AUTRES SANS ACTIVITE PROF.",
                                                                             "CADRES ET PROF INTELLECTUELLES", "EMPLOYES", "OUVRIERS", "PROFESSIONS INTERMEDIAIRES", "RETRAITES"),
                                                                 multiple = TRUE, selectize = TRUE)
                                              )
                                       )
                                     ),
                                     hr(),
                                     fluidRow(
                                       uiOutput("criteresProspectionTitre"),
                                       uiOutput("criteresProspectionDesc")
                                     ),
                                     hr(),
                                     fluidRow(
                                       uiOutput("champActionProspectionTitre"),
                                       numericInput(inputId = "champActionVal", label = "", value = 5, min = 0, max = 100, step = 1), br(),
                                       actionButton(inputId = "btnChampAction", label = "Lancer la prospection")
                                       ),
                                     hr()
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
                            
                   ),
                   # TO DO
                   tabPanel("A propos")
))