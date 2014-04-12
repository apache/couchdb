// Licensed under the Apache License, Version 2.0 (the 'License'); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an 'AS IS' BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

define([
       'addons/logs/resources',
       'testUtils'
], function (Log, testUtils) {
  var assert = testUtils.assert;

  describe('Logs Resources', function () {

    describe('Log Parser', function () {
      it('parses GET messages', function () {
        var parsedLog,
            collection,
            log = '[Sat, 12 Apr 2014 13:02:58 GMT] [info] [<0.3097.2>] 127.0.0.1 - - GET /_session 200\n' +
                  '[Sat, 12 Apr 2014 13:02:58 GMT] [info] [<0.3098.2>] 127.0.0.1 - - GET /_session 200\n' +
                  '[Sat, 12 Apr 2014 13:02:58 GMT] [info] [<0.3099.2>] 127.0.0.1 - - GET / 200\n';

        collection = new Log.Collection();
        parsedLog = collection.parse(log);
        assert.equal(parsedLog[0].date, 'Sat, 12 Apr 2014 13:02:58 GMT');
        assert.equal(parsedLog[0].args, '127.0.0.1 - - GET / 200');
      });

      it('parses GET messages with erlang errors', function () {
        var parsedLog,
            collection,
            log = '[Sat, 12 Apr 2014 13:14:15 GMT] [info] [<0.9491.2>] Retrying GET to http://176.9.4.195/registry/genstatic?revs=true&open_revs=%5B%224-a9be203658a59fd2116ae9acbd10f0de%22%5D&latest=true in 1.0 seconds due to error {error,\n' +
                  '                                                                                                                                                                                      {error,\n' +
                  '                                                                                                                                                                                       connection_closing}}\n' +
                  '[Sat, 12 Apr 2014 13:14:15 GMT] [error] [<0.9499.2>] Replicator, request GET to "http://176.9.4.195/registry/google-openid?revs=true&open_revs=%5B%2219-380884ba97e3d6fc48c8c7db3dc0e91b%22%5D&latest=true" failed due to error {error,connection_closing}\n' +
                  '[Sat, 12 Apr 2014 13:14:15 GMT] [info] [<0.9497.2>] Retrying GET to http://176.9.4.195/registry/google-openid?revs=true&open_revs=%5B%2219-380884ba97e3d6fc48c8c7db3dc0e91b%22%5D&latest=true in 1.0 seconds due to error {error,\n' +
                  '                                                                                                                                                                                           {error,\n' +
                  '                                                                                                                                                                                            connection_closing}}\n' +
                  '[Sat, 12 Apr 2014 13:14:15 GMT] [error] [<0.9507.2>] Replicator, request GET to "http://176.9.4.195/registry/google-openid?revs=true&open_revs=%5B%224-c8ba4809e2cacc1635f8887ec0d8d49a%22%5D&latest=true" failed due to error {error,connection_closing}\n' +
                  '[Sat, 12 Apr 2014 13:14:15 GMT] [info] [<0.9505.2>] Retrying GET to http://176.9.4.195/registry/google-openid?revs=true&open_revs=%5B%224-c8ba4809e2cacc1635f8887ec0d8d49a%22%5D&latest=true in 1.0 seconds due to error {error,\n' +
                  '                                                                                                                                                                                          {error,\n' +
                  '                                                                                                                                                                                           connection_closing}}\n';

        collection = new Log.Collection();
        parsedLog = collection.parse(log);
        assert.equal(parsedLog[1].date, 'Sat, 12 Apr 2014 13:14:15 GMT');
        assert.equal(parsedLog[1].args, 'Replicator, request GET to "http://176.9.4.195/registry/google-openid?revs=true&open_revs=%5B%224-c8ba4809e2cacc1635f8887ec0d8d49a%22%5D&latest=true" failed due to error {error,connection_closing}');
        assert.equal(parsedLog[2].args, 'Retrying GET to http://176.9.4.195/registry/google-openid?revs=true&open_revs=%5B%2219-380884ba97e3d6fc48c8c7db3dc0e91b%22%5D&latest=true in 1.0 seconds due to error {error,{error,connection_closing}}');
      });
    });
  });
});
