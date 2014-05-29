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
       'app',
       'addons/fauxton/components',
       'testUtils',
       'api'
], function (app, Components, testUtils, FauxtonAPI) {
  var assert = testUtils.assert,
      ViewSandbox = testUtils.ViewSandbox;

  describe('FilterView', function () {
    var viewSandbox,
        filterView;

    if (!FauxtonAPI.router.triggerRouteEvent) {
      FauxtonAPI.router.triggerRouteEvent = function () {};
    }

    beforeEach(function () {
      filterView = new Components.FilterView({
        eventNamespace: 'mynamespace'
      });

      viewSandbox = new ViewSandbox();
      viewSandbox.renderView(filterView);
    });

    afterEach(function () {
      viewSandbox.remove();
    });

    it('should add filter markup', function () {
      filterView.$('[name="filter"]').val('i was a lonely filter');
      filterView.$('.js-filter-form').submit();

      filterView.$('[name="filter"]').val('i am a filter');
      filterView.$('.js-filter-form').submit();
      assert.equal(2, filterView.$('.js-remove-filter').length);
    });

    it('should remove filter markup', function () {
      filterView.$('[name="filter"]').val('i was a lonely filter');
      filterView.$('.js-filter-form').submit();
      filterView.$('[name="filter"]').val('i am a filter');
      filterView.$('.js-filter-form').submit();

      filterView.$('.js-remove-filter').click();

      assert.equal(0, filterView.$('.js-remove-filter').length);
    });

    it('should not add empty filters', function () {
      filterView.$('[name="filter"]').val('');
      filterView.$('.js-filter-form').submit();
      assert.equal(0, filterView.$('.js-remove-filter').length);
    });

    it('should not add tooltips by default', function () {
      assert.equal(0, filterView.$('.js-filter-tooltip').length);
    });

    it('should add tooltips when a text for it is defined', function () {
      filterView = new Components.FilterView({
        eventNamespace: 'mynamespace',
        tooltipText: 'ente ente'
      });
      viewSandbox.renderView(filterView);
      assert.equal(1, filterView.$('.js-filter-tooltip').length);
    });
  });
});
