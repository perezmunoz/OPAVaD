/*
// Fonction affichant la table de navigation lors de la connexion à l'application
$(document).ready(function(){
  $("body").on('click', "#Login", function(){
    $("#opavad").parentsUntil('.navbar').parent().show();
    $("#graphique-tab").show();
    $('div.login').remove();
  });
//document.getElementById('opavad').parentNode.parentNode.parentNode.style.display = 'block';
});

// Schéma de l'algorithme : NOT WORKING
// on.click
  // if (USER$Logged est TRUE)
    // Si oui
      // On attend la fin de chargement, ie que class='shiny-busy' redevienne class=''
      // On affiche le tout (cf code ci-dessus)
    // Si non
      // On affiche rien, sauf le message d'erreur à partir de R
  // end if
// end on.click

$(document).ready(function(){
  $('body').on('click','#Login', function(){
    function checkConnexion(){
      if(window.connexion == false){
        console.log("La connexion est false");
        // Si la connexion n'est pas validée on ne fait rien. R se charge d'afficher à l'écran un message d'erreur
        setTimeout("checkConnexion();",100);
        //return;
      } else {
        console.log("La connexion est true")
        $('div.login').remove();
        while($('html').attr('class')=='shiny-busy') {
          console.log("On est dans le while")
          // On attend que la page se construise
        }
        // Quand la page a terminé de charger, on l'affiche en one shot
        $("#opavad").parentsUntil('.navbar').parent().show();
        $("#graphique-tab").show();
      }
   }
  });
});


function checkConnexion(){
      if(window.connexion == false){
        // Si la connexion n'est pas validée on ne fait rien. R se charge d'afficher à l'écran un message d'erreur
        setTimeout("checkConnexion();",100);
        return;
      }
}

$(document).ready(function(){
  $('body').on('click','#Login', function(){
    // On appel la fonction bloquant l'exécution sur le test de la variable connexion
    if(window.connexion == false){
        setTimeout(function(){if(window.connexion==false)console.log("On attend")},100);
    }
    // Lorsque connexion = true
    $('div.login').remove();
    while($('html').attr('class')=='shiny-busy') {
      // On attend que la page se construise
    }
    // Quand la page a terminé de charger, on l'affiche en one shot
    $("#opavad").parentsUntil('.navbar').parent().show();
    $("#graphique-tab").show();
  });
});

checkConnexion function(){
  if(window.connexion == false){
        // Si la connexion n'est pas validée on ne fait rien. R se charge d'afficher à l'écran un message d'erreur
        window.setTimeout("checkConnexion();",100);
        return;
  } else {console.log("Connexion is true");}
}

function checkConnexion(){
  if(window.connexion == false){
        // Si la connexion n'est pas validée on ne fait rien. R se charge d'afficher à l'écran un message d'erreur
        window.setTimeout("checkConnexion();",100);
        return;
  } else {console.log("Connexion is true");}
}
  // Lorsque connexion = true
  $('div.login').remove();
  while($('html').attr('class')=='shiny-busy') {
    // On attend que la page se construise
  }
  // Quand la page a terminé de charger, on l'affiche en one shot
  $("#opavad").parentsUntil('.navbar').parent().show();
  $("#graphique-tab").show();
}

$(document).ready(function(){
  $('body').on('click','#Login', function(){
    function checkConnexion(){
      while(!window.connexion==false){
        setTimeout("checkConnexion()",200);
      }
    }
    while($('html').attr('class')=='shiny-busy'){
      // Wait
    }
    // Quand la page a terminé de charger, on l'affiche en one shot
    $('div.login').remove();
    $("#opavad").parentsUntil('.navbar').parent().show();
    $("#graphique-tab").show();
  });
});
*/