
// Require index.html so it gets copied to dist
require('./index.html');

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
