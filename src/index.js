
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
  console.log("generateC3", object.domId);
  var chart = c3.generate({
    bindto: "#" + object.domId,
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

  charts[object.domId] = chart;
  updateChart(chart, object.data, object.variableTitle);
  app.ports.charts.send(Object.keys(charts));
});

function updateChart(chart, realData, variableTitle) {
  var dataRows = [].concat([['x', 'Nettonåverdi']], realData);
  chart.axis.labels({x: variableTitle});
  chart.load(
    {
      rows: dataRows,
    });

}

app.ports.updateC3.subscribe(function (object) {
  console.log('update', object);
  var chart = charts[object.domId];
  updateChart(chart, object.data, object.variableTitle);  
});



app.ports.destroyC3.subscribe(function (domId) {
  console.log("destroyC3", domId);
  charts[domId].destroy();
  delete charts[domId];
  console.log("remaining charts", charts);
  app.ports.charts.send(Object.keys(charts));
});
