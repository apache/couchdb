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
        'api',
        'addons/activetasks/views',
        'addons/activetasks/resources',
        'testUtils'
], function (FauxtonAPI, Views, Activetasks, testUtils) {
  var assert = testUtils.assert,
      ViewSandbox = testUtils.ViewSandbox;

  describe("TabMenu", function () {
    var tabMenu;

    beforeEach(function () {
      tabMenu = new Views.TabMenu({});
    });

    describe("on change polling rate", function () {
      var viewSandbox;
      beforeEach(function () {
        viewSandbox = new ViewSandbox();
        viewSandbox.renderView(tabMenu);
      });

      afterEach(function () {
        viewSandbox.remove();
      });

      it("Should set polling rate", function () {
        var $range = tabMenu.$('#pollingRange');
        $range.val(15);
        $range.trigger('change');

        assert.equal(tabMenu.$('span').text(), 15);
      });

      it("Should clearInterval", function () {
        var $range = tabMenu.$('#pollingRange');
        var clearIntervalMock = sinon.spy(window,'clearInterval');
        $range.trigger('change');

        assert.ok(clearIntervalMock.calledOnce);

      });

      it("Should trigger update:poll event", function () {
        var spy = sinon.spy();
        Views.Events.on('update:poll', spy);
        var $range = tabMenu.$('#pollingRange');
        $range.trigger('change');

        assert.ok(spy.calledOnce);
      });

    });

    describe('on request by type', function () {
      var viewSandbox, mainView;
      beforeEach(function () {

        mainView = new Views.View({
          collection: new Activetasks.AllTasks(),
          currentView: "all"
        });

        viewSandbox = new ViewSandbox();
        viewSandbox.renderView(tabMenu);
        viewSandbox.renderView(mainView);
      });

      afterEach(function () {
        viewSandbox.remove();
      });

      it("should set the filter the main-view", function () {
        var $rep = tabMenu.$('li[data-type="replication"]');
        $rep.click();
        assert.equal("replication", mainView.filter);
      });

      it("should set correct active tab", function () {
        var $rep = tabMenu.$('li[data-type="replication"]');
        $rep.click();
        assert.ok($rep.hasClass('active'));
      });
    });

  });

  describe('DataSection', function () {
    var viewSandbox, mainView;
    beforeEach(function () {
      mainView = new Views.View({
        collection: new Activetasks.AllTasks(),
        currentView: "all"
      });

      viewSandbox = new ViewSandbox();
      viewSandbox.renderView(mainView);
    });

    afterEach(function () {
      viewSandbox.remove();
    });

    describe('#setPolling', function () {

      it('Should set polling interval', function () {
        var spy = sinon.spy(window, 'setInterval');
        mainView.setPolling();
        assert.ok(spy.calledOnce);
      });

    });
  });
});
