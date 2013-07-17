define([
       'addons/logs/base',
       'chai'
], function (Log, chai) {
  var expect = chai.expect;

  describe('Logs Addon', function(){

    describe('Log Model', function () {
      var log;

      beforeEach(function () {
        log = new Log.Model({
          log_level: 'DEBUG',
          pid: '1234',
          args: 'testing 123',
          date: (new Date()).toString()
        });
      });

      it('should have a log level', function () {
        expect(log.logLevel()).to.equal('DEBUG');
      });

    });
  });
});
