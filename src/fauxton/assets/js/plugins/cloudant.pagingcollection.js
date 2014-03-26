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

(function(root, factory) {
  "use strict";
  // start with AMD, so paginate could then be used by ``var paginate = require('paginate');``
  if (typeof define === 'function' && define.amd) {
    define(['underscore', 'backbone', 'jquery'], function(_, Backbone, $) {
      // Export global even in AMD case in case this script is loaded with
      // others that may still expect a global paginate.
      return factory(root, null, _, Backbone, $.param);
    });

  // Next check for Node.js or CommonJS. Also look to see if either
  // underscore or lodash are the modules being used
  } else if (typeof exports !== 'undefined') {
    var Backbone = require('Backbone'),
        $param = require('querystring').stringify,
        _;
    try {
      _ = require('underscore');
    } catch(e) {
      _ = require('lodash');
    }
    factory(root, exports, _, Backbone, $param);

  // Finally, register as a browser global.
  } else {
    root.PagingCollection = factory(root, {}, root._, root.Backbone, root.$.param);
  }

}(this, function(root, exports, _, Backbone, $param) {
  "use strict";

  //PagingCollection
  //----------------

  // A PagingCollection knows how to build appropriate requests to the
  // CouchDB-like server and how to fetch. The Collection will always contain a
  // single page of documents.

  var PagingCollection = Backbone.Collection.extend({

    // initialize parameters and page size
    constructor: function() {
      Backbone.Collection.apply(this, arguments);
      this.configure.apply(this, arguments);
    },

    configure: function(collections, options) {
      var querystring = _.result(this, "url").split("?")[1] || "";
      this.paging = _.defaults((options.paging || {}), {
        defaultParams: _.defaults({}, options.params, this._parseQueryString(querystring)),
        hasNext: false,
        hasPrevious: false,
        params: {},
        pageSize: 20,
        direction: undefined
      });

      this.paging.params = _.clone(this.paging.defaultParams);
      this.updateUrlQuery(this.paging.defaultParams);
    },

    calculateParams: function(currentParams, skipIncrement, limitIncrement) {

      var params = _.clone(currentParams);
      params.skip = (parseInt(currentParams.skip, 10) || 0) + skipIncrement;

      // guard against hard limits
      if(this.paging.defaultParams.limit) {
        params.limit = Math.min(this.paging.defaultParams.limit, params.limit);
      }
      // request an extra row so we know that there are more results
      params.limit = limitIncrement + 1;
      // prevent illegal skip values
      params.skip = Math.max(params.skip, 0);

      return params;
    },

    pageSizeReset: function(pageSize, opts) {
      var options = _.defaults((opts || {}), {fetch: true});
      this.paging.direction = undefined;
      this.paging.pageSize = pageSize;
      this.paging.params = this.paging.defaultParams;
      this.paging.params.limit = pageSize;
      this.updateUrlQuery(this.paging.params);
      if (options.fetch) {
        return this.fetch();
      }
    },

    _parseQueryString: function(uri) {
      var queryString = decodeURI(uri).split(/&/);

      return _.reduce(queryString, function (parsedQuery, item) {
          var nameValue = item.split(/=/);
          if (nameValue.length === 2) {
            parsedQuery[nameValue[0]] = nameValue[1];
          }

          return parsedQuery;
      }, {});
    },

    _iterate: function(offset, opts) {
      var options = _.defaults((opts || {}), {fetch: true});

      this.paging.params = this.calculateParams(this.paging.params, offset, this.paging.pageSize);

      // Fetch the next page of documents
      this.updateUrlQuery(this.paging.params);
      if (options.fetch) {
        return this.fetch({reset: true});
      }
    },

    // `next` is called with the number of items for the next page.
    // It returns the fetch promise.
    next: function(options){
      this.paging.direction = "next";
      return this._iterate(this.paging.pageSize, options);
    },

    // `previous` is called with the number of items for the previous page.
    // It returns the fetch promise.
    previous: function(options){
      this.paging.direction = "previous";
      return this._iterate(0 - this.paging.pageSize, options);
    },

    shouldStringify: function (val) {
      try {
        JSON.parse(val);
        return false;
      } catch(e) {
        return true;
      }
    },

    // Encodes the parameters so that couchdb will understand them
    // and then sets the url with the new url.
    updateUrlQuery: function (params) {
      var url = _.result(this, "url").split("?")[0];

      _.each(['startkey', 'endkey', 'key'], function (key) {
        if (_.has(params, key) && this.shouldStringify(params[key])) {
          params[key] = JSON.stringify(params[key]);
        }
      }, this);

      this.url = url + '?' + $param(params);
    },

    fetch: function () {
      // if this is a fetch for the first time, fetch one extra to see if there is a next
      if (!this.paging.direction && this.paging.params.limit > 0) {
        this.paging.direction = 'fetch';
        this.paging.params.limit = this.paging.params.limit + 1;
        this.updateUrlQuery(this.paging.params);
      }

      return Backbone.Collection.prototype.fetch.apply(this, arguments);
    },

    parse: function (resp) {
      var rows = resp.rows;

      this.paging.hasNext = this.paging.hasPrevious = false;

      this.viewMeta = {
        total_rows: resp.total_rows,
        offset: resp.offset,
        update_seq: resp.update_seq
      };

      var skipLimit = this.paging.defaultParams.skip || 0;
      if(this.paging.params.skip > skipLimit) {
        this.paging.hasPrevious = true;
      }

      if(rows.length === this.paging.pageSize + 1) {
        this.paging.hasNext = true;

        // remove the next page marker result
        rows.pop();
        this.viewMeta.total_rows = this.viewMeta.total_rows - 1;
      }
      return rows;
    },

    hasNext: function() {
      return this.paging.hasNext;
    },

    hasPrevious: function() {
      return this.paging.hasPrevious;
    }
  });


  if (exports) {
    // Overload the Backbone.ajax method, this allows PagingCollection to be able to
    // work in node.js
    exports.setAjax = function (ajax) {
      Backbone.ajax = ajax;
    };

    exports.PagingCollection = PagingCollection;
  }

  return PagingCollection;
}));

