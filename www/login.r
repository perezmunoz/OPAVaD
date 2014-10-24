###########################
###### Log in module ######
###########################

# Variable réactive nécessaire à la transition de l'interface de connexion et l'interface mon fond de commerce
USER <- reactiveValues(Logged = Logged)
# session$sendCustomMessage(type='jsCode', list(value = "var connexion = false;"))

# Interface de connexion
output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
    wellPanel(
      isolate({textInput(inputId = "userName", label = "Num\u00E9ro de Siret :", value = "47964299300022")}),
      br(),
      actionButton("Login", "Connexion")
    )
  }
})

# Vérification de la connexion
output$pass <- renderText({
  if (USER$Logged == FALSE) {
    if (!is.null(input$Login)) {
      if(input$Login>0) {
        isolate({
          KEY <<- index[index$siret == input$userName, ]
        })
        if(nrow(KEY) != 0) {
          if (KEY$ca == 'O') {
#             session$sendCustomMessage(type='jsCode', list(value = "connexion = true;"))
            session$sendCustomMessage(type='jsCode', list(value = "window.connexion = true;"))
            USER$Logged <- TRUE
            "Connexion validée"
          } else  {
            print("toto1")
            "Num\u00E9ro de Siret incorrecte !"
          }
        } else {
          print("toto2")
          "Num\u00E9ro de Siret incorrecte !"
        }
      }
    }
  }
})