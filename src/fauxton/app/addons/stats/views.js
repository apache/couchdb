// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

define([
  "app",

  "api",
  'addons/stats/resources',
  "d3",
  "nv.d3"

],

function(app, FauxtonAPI, Stats, d3, nv) {
  var Views = {},
      datatypeEventer = {};

  _.extend(datatypeEventer, Backbone.Events);

  Views.Legend = FauxtonAPI.View.extend({
    tagName: 'ul',
    template: "addons/stats/templates/legend",

    serialize: function () {
      return {
        legend_items: this.collection.toJSON()
      };
    }
  });

  Views.Pie = FauxtonAPI.View.extend({
    className: "datatype-section",
    template: 'addons/stats/templates/pie_table',

    initialize: function(args){
      this.datatype = args.datatype;
    },

    serialize: function() {
      return {
        statistics: this.collection.where({type: this.datatype}),
        datatype: this.datatype
      };
    },

    afterRender: function(){
        var collection = this.collection,
            chartelem = "#" + this.datatype + '_graph',
            series = _.map(this.collection.where({type: this.datatype}),
          function(d, counter){
            // TODO: x should be a counter
            var point = {
              y: d.get("sum") || 0,
              key: d.id
            };
            return point;
          }
        );

        series = _.filter(series, function(d){return d.y > 0;});
        series = _.sortBy(series, function(d){return -d.y;});

        nv.addGraph(function() {
            var width = 440,
                height = 440;

            var chart = nv.models.pieChart()
                .x(function(d) { return d.key; })
                .y(function(d) { return d.y; })
                .showLabels(true)
                .showLegend(false)
                .color(d3.scale.category10().range())
                .width(width)
                .height(height);

              d3.select(chartelem)
                  .datum(series)
                .transition().duration(300)
                  .attr('width', width)
                  .attr('height', height)
                  .call(chart);

            return chart;
        });

      this.$el.addClass(this.datatype + '_section');
    }
  });

  Views.StatSelect = FauxtonAPI.View.extend({
    className: 'nav nav-tabs nav-stacked',
    tagName: 'ul',

    template: "addons/stats/templates/statselect",

    initialize: function (options) {
      this.rows = [];
    },

    events: {
      'click .datatype-select': "datatype_selected"
    },

    serialize: function () {
      return {
        datatypes: _.uniq(this.collection.pluck("type"))
      };
    },

    afterRender: function () {
      this.$('.datatype-select').first().addClass('active');
    },

    datatype_selected: function (event) {
      var $target = $(event.currentTarget);

      event.preventDefault();
      event.stopPropagation();
      this.$('.datatype-select').removeClass('active');
      $target.addClass('active');
      datatypeEventer.trigger('datatype-select', $target.attr('data-type-select'));
    }
  });

  Views.Statistics = FauxtonAPI.View.extend({
    template: "addons/stats/templates/stats",

    initialize: function (options) {
      this.rows = [];
      datatypeEventer.on('datatype-select', this.display_datatype, this);
    },

    serialize: function () {
      return {
        datatypes: _.uniq(this.collection.pluck("type"))
      };
    },

    beforeRender: function () {
      _.each(_.uniq(this.collection.pluck("type")), function(datatype) {
        this.rows[datatype] = this.insertView(".datatypes", new Views.Pie({
          collection: this.collection,
          datatype: datatype
        }));
      }, this);
    },

    afterRender: function () {
      this.$('.datatype-section').hide().first().toggle();
    },

    display_datatype: function (datatype) {
      this.$('.datatype-section').hide();
      this.$('.' + datatype + '_section').show();
    }
  });

  Stats.Views = Views;

  return Stats;
});
