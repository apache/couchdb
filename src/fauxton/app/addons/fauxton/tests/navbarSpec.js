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
       'addons/fauxton/base',
      'testUtils'
], function (Fauxton, testUtils) {
  var assert = testUtils.assert,
      NavBar = Fauxton.NavBar;

  describe('NavBar', function () {

    describe('adding links', function () {
      var navBar;

      beforeEach(function () {
        navBar = new NavBar();
        navBar.navLinks = [];
        navBar.bottomNavLinks = [];
        navBar.footerNavLinks = [];
      });

      it('Should add link to navlinks', function () {
        navBar.addLink({href: '#/test', title: 'Test Title'});

        assert.equal(navBar.navLinks.length, 1);
        assert.equal(navBar.footerNavLinks.length, 0);
        assert.equal(navBar.bottomNavLinks.length, 0);
      });

      it('Should add link to bottom links', function () {
        navBar.addLink({href: '#/test', bottomNav: true, title: 'Test Title'});

        assert.equal(navBar.bottomNavLinks.length, 1);
        assert.equal(navBar.navLinks.length, 0);
        assert.equal(navBar.footerNavLinks.length, 0);
      });

      it('Should add link to footer links', function () {
        navBar.addLink({href: '#/test', footerNav: true, title: 'Test Title'});

        assert.equal(navBar.footerNavLinks.length, 1);
        assert.equal(navBar.bottomNavLinks.length, 0);
        assert.equal(navBar.navLinks.length, 0);
      });
    });

    describe('removing links', function () {
      var navBar;

      beforeEach(function () {
        navBar = new NavBar();
        navBar.navLinks = [];
        navBar.bottomNavLinks = [];
        navBar.footerNavLinks = [];
        navBar.addLink({
          href: '#/test', 
          footerNav: true, 
          title: 'Test Title Footer'
        });

        navBar.addLink({
          href: '#/test', 
          bottomNav: true, 
          title: 'Test Title Bottom'
        });

        navBar.addLink({
          href: '#/test', 
          title: 'Test Title'
        });
      });

      it("should remove links from list", function () {
        navBar.removeLink({
          title: 'Test Title Footer',
          footerNav: true
        });

        assert.equal(navBar.footerNavLinks.length, 0);
        assert.equal(navBar.bottomNavLinks.length, 1);
        assert.equal(navBar.navLinks.length, 1);
      });

      it("Should call render after removing links", function () {
        var renderSpy = sinon.stub(navBar,'render');

        navBar.removeLink({
          title: 'Test Title Footer',
          footerNav: true
        });

        assert.ok(renderSpy.calledOnce);
      });

    });
  });

});
