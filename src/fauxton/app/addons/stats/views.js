define([
  "app",

  "api",
  "d3",
  "rickshaw"
],

function(app, FauxtonAPI) {

  Views = {};

  Views.Pie = FauxtonAPI.View.extend({
    initialize: function(args){
      this.template = "addons/stats/templates/" + args.template;
      this.datatype = args.datatype;
    },
    serialize: function() {
      return {
        statistics: this.collection.where({type: this.datatype})
      };
    },
    afterRender: function(){
      var collection = this.collection;
      var chartelem = "#" + this.datatype;
      var series = _.map(this.collection.where({type: this.datatype}),
        function(d){
          // TODO: x should be a counter
          var point = {x: 0, y: d.get("sum") || 0, label: d.id};
          return point;
        }
      );
      series = _.filter(series, function(e){return e.y > 0;});
      var graph = new Rickshaw.Graph({
          element: document.querySelector(chartelem),
          width: 300,
          height: 300,
          renderer: "pie",
          outerRadius: 150,
          scheme: "spectrum14",
          series: [{
            data: series
          }]
      });

      graph.render();
    }
  });

  return Views;
});