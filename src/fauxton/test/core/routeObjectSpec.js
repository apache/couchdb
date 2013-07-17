define([
       'api',
      'testUtils'
], function (FauxtonAPI, testUtils) {
  var assert = testUtils.assert,
      RouteObject = FauxtonAPI.RouteObject;

  describe('RouteObjects', function () {

    describe('renderWith', function () {
      var TestRouteObject, testRouteObject, mockLayout;

      beforeEach(function () {
        TestRouteObject = RouteObject.extend({
          crumbs: ['mycrumbs']
        });

        testRouteObject = new TestRouteObject();

        // Need to find a better way of doing this
        mockLayout = {
          setTemplate: sinon.spy(),
          clearBreadcrumbs: sinon.spy(),
          setView: sinon.spy(),
          renderView: sinon.spy(),
          hooks: [],
          setBreadcrumbs: sinon.spy()
        };

      });

      it('Should set template for first render ', function () {
        testRouteObject.renderWith('the-route', mockLayout, 'args');

        assert.ok(mockLayout.setTemplate.calledOnce, 'setTempalte was called');
      });

      it('Should not set template after first render', function () {
        testRouteObject.renderWith('the-route', mockLayout, 'args');

        testRouteObject.renderWith('the-route', mockLayout, 'args');

        assert.ok(mockLayout.setTemplate.calledOnce, 'SetTemplate not meant to be called');
      });

      it('Should clear breadcrumbs', function () {
        testRouteObject.renderWith('the-route', mockLayout, 'args');
        assert.ok(mockLayout.clearBreadcrumbs.calledOnce, 'Clear Breadcrumbs called');
      });

      it('Should set breadcrumbs when breadcrumbs exist', function () {
        testRouteObject.renderWith('the-route', mockLayout, 'args');
        assert.ok(mockLayout.setBreadcrumbs.calledOnce, 'Set Breadcrumbs was called');
      });

      it("Should call establish of routeObject", function () {
        var establishSpy = sinon.spy(testRouteObject,"establish");

        testRouteObject.renderWith('the-route', mockLayout, 'args');
        assert.ok(establishSpy.calledOnce, 'Calls establish');
      });

      it("Should render views", function () {
        var view = new FauxtonAPI.View(),
            getViewsSpy = sinon.stub(testRouteObject,"getViews"),
            viewSpy = sinon.stub(view, "establish");
        
        sinon.stub(view, "hasRendered").returns(false);
        getViewsSpy.returns({'#view': view});

        testRouteObject.renderWith('the-route', mockLayout, 'args');
        assert.ok(viewSpy.calledOnce, 'Should render view');
      });

      it("Should not re-render a view", function () {
        var view = new FauxtonAPI.View(),
            getViewsSpy = sinon.stub(testRouteObject,"getViews"),
            viewSpy = sinon.stub(view, "establish");
        
        sinon.stub(view, "hasRendered").returns(true);
        getViewsSpy.returns({'#view': view});

        testRouteObject.renderWith('the-route', mockLayout, 'args');
        assert.notOk(viewSpy.calledOnce, 'Should render view');
      });
    });

  });


});
