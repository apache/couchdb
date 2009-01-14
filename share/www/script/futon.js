// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License.  You may obtain a copy
// of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
// License for the specific language governing permissions and limitations under
// the License.

(function($) {

  function Navigation() {
    var nav = this;
    this.loaded = false;
    this.eventHandlers = {
      load: []
    };

    this.ready = function(callback) {
      if (callback) {
        if (this.loaded) {
          callback.apply(this);
        }
        this.eventHandlers["load"].push(callback);
      } else {
        this.loaded = true;
        callbacks = this.eventHandlers["load"];
        for (var i = 0; i < callbacks.length; i++) {
          callbacks[i].apply(this);
        }
      }
    }

    this.addDatabase = function(name) {
      var recentDbs = $.cookies.get("recent", "").split(",");
      if ($.inArray(name, recentDbs) == -1) {
        recentDbs.unshift(name);
        if (recentDbs.length > 10) recentDbs.length = 10;
        $.cookies.set("recent", recentDbs.join(","));
        this.updateDatabases();
      }
    }

    this.removeDatabase = function(name) {
      // remove database from recent databases list
      var recentDbs = $.cookies.get("recent", "").split(",");
      var recentIdx = $.inArray(name, recentDbs);
      if (recentIdx >= 0) {
        recentDbs.splice(recentIdx, 1);
        $.cookies.set("recent", recentDbs.join(","));
        this.updateDatabases();
      }
    }

    this.updateDatabases = function() {
      var selection = null;
      $("#dbs .selected a").each(function() {
        selection = [this.pathname, this.search];
      });
      $("#dbs").empty();
      var recentDbs = $.cookies.get("recent", "").split(",");
      recentDbs.sort();
      $.each(recentDbs, function(idx, name) {
        if (name) {
          $("#dbs").append("<li><a href='database.html?" +
            encodeURIComponent(name) + "'>" + name +
            "<button class='remove' title='Remove from list' value='" + name + "'></button>" +
            "</a></li>");
        }
      });
      if (selection) {
        this.updateSelection(selection[0], selection[1]);
      }
      $("#dbs button.remove").click(function() {
        nav.removeDatabase(this.value);
        return false;
      });
    }

    this.updateSelection = function(path, queryString) {
      function fixupPath(path) { // hack for IE/Win
        return (path.charAt(0) != "/") ? ("/" + path) : path;
      }
      if (!path) {
        path = location.pathname;
        if (!queryString) {
          queryString = location.search;
        }
      } else if (!queryString) {
        queryString = "";
      }
      var href = fixupPath(path + queryString);
      $("#nav li").removeClass("selected");
      $("#nav li a").each(function() {
        if (fixupPath(this.pathname) + this.search != href) return;
        $(this).parent("li").addClass("selected").parents("li").addClass("selected");
      });
    }

  }

  $.futon = $.futon || {};
  $.extend($.futon, {
    navigation: new Navigation()
  });

  $(document)
    .ajaxStart(function() { $(this.body).addClass("loading"); })
    .ajaxStop(function() { $(this.body).removeClass("loading"); });

  $(function() {
    document.title = "Apache CouchDB - Futon: " + document.title;
    $.get("_sidebar.html", function(resp) {
      $(resp).insertAfter("#wrap");

      $.futon.navigation.updateDatabases();
      $.futon.navigation.updateSelection();
      $.futon.navigation.ready();

      $.couch.info({
        success: function(info, status) {
          $("#version").text(info.version);
        }
      });
    });
  });

})(jQuery);
