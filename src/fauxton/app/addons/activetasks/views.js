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
  "addons/activetasks/resources"
],

function (app, FauxtonAPI, activetasks) {

  var Views = {};

	Views.TabMenu = FauxtonAPI.View.extend({
		template: "addons/activetasks/templates/tabs",
		events: {
			"click .task-tabs li a": "requestByType"
		},
		establish: function(){
			return [this.model.fetch({reset: true})];
		},
		serialize: function(){
			return {
				filters: this.model.alltypes
			};
		},
		requestByType: function(e){
			e.preventDefault();
			var currentTarget = e.currentTarget;
					datatype = $(currentTarget).attr("data-type");

			$('.task-tabs').find('li').removeClass('active');
			$(currentTarget).parents('li').addClass('active');
			this.model.changeView(datatype);
		}
	});

	Views.DataSection = FauxtonAPI.View.extend({
		initialize: function(){
			this.listenToOnce(this.model, "change", this.showData); //check why I needed to do this
		},
		showData: function(){
			var that = this;
			$(this.el).empty();
			if (this.model.getCurrentViewData().length > 0) {
				this.model.getCurrentViewData().each(function(model) {
					var view = that.insertView( new Views.Taskdetail({ 
						model: model
					}));
					view.render();
				});
			} else {
				var currentView = this.model.get('currentView').replace('_',' ');
				$(this.el).html("<p> There are no active tasks for "+currentView+' right now.</p>');
			}
		},
		establish: function(){
			return [this.model.fetch()];
		},
		afterRender: function(){
			this.listenTo(this.model, "change", this.showData);
			var that = this;
			setInterval(function() {
				that.establish();
			}, 3000);
		}
	});

	Views.Taskdetail = FauxtonAPI.View.extend({
		template: "addons/activetasks/templates/detail",
		serialize: function(){
			return {
				model: this.model
			};
		}
	});

 
  return Views;
});
