###########################
###### Log in module ######
###########################

# Variable réactive nécessaire à la transition de l'interface de connexion et l'interface mon fond de commerce
USER <- reactiveValues(Logged = Logged)

# Interface de connexion
output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
    wellPanel(
      isolate({textInput(inputId = "userName", label = "Num\u00E9ro de Siret :", value = "49343803000030")}),
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
            USER$Logged <- TRUE
            "Connexion validée"
          } else  {
            "Num\u00E9ro de Siret incorrecte !"
          }
        } else {
          "Num\u00E9ro de Siret incorrecte !"
        }
      }
    }
  }
})