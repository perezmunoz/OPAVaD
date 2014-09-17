$(document).ready(function(){
  $("body").on('click', "#Login", function(){
    $("#opavad").parentsUntil('.navbar').parent().show();
    $("#graphique-tab").show();
    $('div.login').remove();
  });
//document.getElementById('opavad').parentNode.parentNode.parentNode.style.display = 'block';
});
