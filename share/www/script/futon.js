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
          $("#dbs").append("<li>" +
            "<button class='remove' title='Remove from list' value='" + name + "'></button>" +
            "<a href='database.html?" + encodeURIComponent(name) + "' title='" + name + "'>" + name +
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

    this.toggle = function(speed) {
      if (speed === undefined) {
        speed = 500;
      }
      var sidebar = $("#sidebar").stop(true, true);
      var hidden = !$(sidebar).is(".hidden");

      $("#wrap").animate({
        marginRight: hidden ? 0 : 210
      }, speed, function() {
        $(document.body).toggleClass("fullwidth", hidden);
      });
      sidebar.toggleClass("hidden").animate({
        width: hidden ? 26 : 210,
        height: hidden ? $("h1").outerHeight() - 1 : "100%",
        right: hidden ? 0 : -210
      }, speed).children(":not(#sidebar-toggle)").animate({
        opacity: "toggle"
      }, speed);
      $("h1").animate({marginRight: hidden ? 26 : 0}, speed);

      $("#sidebar-toggle")
        .attr("title", hidden ? "Show Sidebar" : "Hide Sidebar");
      $.cookies.set("sidebar", hidden ? "hidden" : "show");
    };
  }

  $.futon = $.futon || {};
  $.extend($.futon, {
    navigation: new Navigation()
  });

  $.fn.addPlaceholder = function(text) {
    return this.each(function() {
      var input = $(this);
      if ($.browser.safari) {
        input.attr("placeholder", text);
        return;
      }
      input.blur(function() {
        if ($.trim(input.val()) == "") {
          input.addClass("placeholder").val(text);
        } else {
          input.removeClass("placeholder");
        }
      }).triggerHandler("blur")
      input.focus(function() {
        if (input.is(".placeholder")) {
          input.val("").removeClass("placeholder");
        }
      });
      $(this.form).submit(function() {
        if (input.is(".placeholder")) {
          input.val("");
        }
      });
    });
  }

  $(document)
    .ajaxStart(function() { $(this.body).addClass("loading"); })
    .ajaxStop(function() { $(this.body).removeClass("loading"); });

  $(function() {
    document.title = "Apache CouchDB - Futon: " + document.title;
    if ($.cookies.get("sidebar") == "hidden") {
      // doing this as early as possible prevents flickering
      $(document.body).addClass("fullwidth");
    }
    $.get("_sidebar.html", function(resp) {
      $("#wrap").append(resp)
        .find("#sidebar-toggle").click(function(e) {
            $.futon.navigation.toggle(e.shiftKey ? 2500 : 500);
            return false;
          });
      if ($.cookies.get("sidebar") == "hidden") {
        $.futon.navigation.toggle(0);
      }

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
