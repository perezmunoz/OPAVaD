###########################
###### Log in module ######
###########################

USER <- reactiveValues(Logged = Logged)

output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
    wellPanel(
      textInput("userName", "Num\u00E9ro de Siret :", "44031147000023"),
      br(),
      actionButton("Login", "Connexion")
    )
  }
})

output$pass <- renderText({
  if (USER$Logged == FALSE) {
    if (!is.null(input$Login)) {
      if (input$Login > 0) {
        
        userId <- isolate(input$userName)
        KEY <<- commercants[commercants$siret == input$userName, ]
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