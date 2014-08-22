setInterval(function(){
  if ($('html').attr('class')=='shiny-busy') {
    setTimeout(function() {
      if ($('html').attr('class')=='shiny-busy') {
        document.getElementById("loading").src = "www/ajaxloader.png";
      }
    }, 1000)
  } else {
    document.getElementById("loading").src = "none";
  }
}, 100)