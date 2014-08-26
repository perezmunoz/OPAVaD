###########################
###### Log in module ######
###########################

# Variable réactive nécessaire à la transition de l'interface connexion à l'interface mon fond de commerce
USER <- reactiveValues(Logged = Logged)

# Interface de connexion
output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
    print("[output$uiLogin] Connexion non établie")
    wellPanel(
      textInput("userName", "Num\u00E9ro de Siret :", "44031147000023"),
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
        
        print("[output$pass] Vérification de la connexion")
        userId <- isolate(input$userName)
        KEY <<- head(commercants[commercants$siret == input$userName, ], 1)
        affiliate <- KEY$affilie_ca == 'O'
        
        if(length(affiliate) != 0) {
          if (affiliate == 'TRUE') {
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