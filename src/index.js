
// Require index.html so it gets copied to dist
require('./index.html');

require("c3/c3.css");

var d3 = require("d3");
var c3 = require("c3");


if (process.env.USE_TILTAK_MAIN) {
  var Elm = require('./TiltakMain.elm');
} else {
  var Elm = require('./Main.elm');
}

var mountNode = document.querySelector('main');

// .embed() can take an optional second argument. This would be an object describing the data we need to start a program, i.e. a userID or some token
var app = Elm.Main.embed(mountNode);


// unused for now
window.matchMedia("print").addListener(function (mediaQueryList) {
   console.log("forandret media", mediaQueryList);
   app.ports.printMediaType.send(mediaQueryList.matches);
});

app.ports.generateC3.subscribe(function (domId) {
  console.log("got here", domId);
  var chart = c3.generate({
    bindto: "#" + domId,
    data: {
      columns: [
        ['data1', 30, 200, 100, 400, 150, 250],
        ['data2', 50, 20, 10, 40, 15, 25]
      ]
    }
});
});
