###########################
###### Log in module ######
###########################

# Variable réactive nécessaire à la transition de l'interface de connexion et l'interface mon fond de commerce
USER <- reactiveValues(Logged = Logged)

# Interface de connexion
output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
    wellPanel(
      textInput("userName", "Num\u00E9ro de Siret :", "44031024100029"),
      br(),
      actionButton("Login", "Connexion")
    )
  }
})

# Vérification de la connexion
output$pass <- renderText({
  if (USER$Logged == FALSE) {
    if (!is.null(input$Login)) {
      if (input$Login > 0) {
        
        KEY <<- index[index$siret == input$userName, ]

        if(length(KEY) != 0) {
          if (KEY$ca == 'O') {
            USER$Logged <- TRUE
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