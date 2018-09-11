var ElmScrollPorts = {};

(function() {
  ElmScrollPorts.subscribe = subscribe;

  function subscribe(app, scrollPortName) {
    if (!scrollPortName) scrollPortName = "scrollTo";

    app.ports[scrollPortName].subscribe(function(id) {
      console.log("scrolling to element id " + id);
      var delay = 300;
      setTimeout(function() {
        var navbarHeight = document.getElementById("navbar").clientHeight;
        var el = document.getElementById(id);
        el.scrollIntoView({
          behaviour: "smooth",
          block: "start"
        });
        window.scrollBy(0, -navbarHeight);
      }, delay);
    })
  }
})();
