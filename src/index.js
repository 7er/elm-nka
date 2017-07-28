
// Require index.html so it gets copied to dist
require('./index.html');
require('./index.scss');

require("c3/c3.css");

var d3 = require("d3");
var c3 = require("c3");


var Elm = require('./Main.elm');

var mountNode = document.querySelector('main');

// .embed() can take an optional second argument. This would be an object describing the data we need to start a program, i.e. a userID or some token
var app = Elm.Main.embed(mountNode);


var charts = {};



app.ports.generateC3.subscribe(function (object) {
  var domId = object.domId;
  var data = object.data;

  console.log("generateC3", domId);
  var chart = c3.generate({
    bindto: "#" + domId,
    type: 'line',
    data: {
      x: 'x',
      columns: [],
    },
    axis: {
      y: {
        label: {
          text: "Nettonåverdi",
          position: 'outer-middle'
        },
        tick: {
          //format: $scope.tickFormat
        }
      },
      x: {
        tick: {
          //format: $scope.tickFormat
        }
      }
    },
    grid: {
      y: {
        lines: [
          {value: 0, text: "Grense for lønnsomhet"}
        ]
      }
    }
  });

  charts[domId] = chart;
  updateChart(chart, data);
});

function updateChart(chart, realData) {
  var dataRows = [].concat([['x', 'Nettonåverdi']], realData);
  chart.axis.labels({x: "Navnet på variabelen vi grafer skal stå her"});
  chart.load(
    {
      rows: dataRows,
    });

}

app.ports.updateC3.subscribe(function (object) {
  console.log('update', object);
  var domId = object.domId;
  var data = object.data;
  var chart = charts[domId];
  updateChart(chart, data);
});



app.ports.destroyC3.subscribe(function (domId) {
  console.log("destroyC3", domId);
  charts[domId].destroy();
  delete charts[domId];
  console.log("remaining charts", charts);
});
