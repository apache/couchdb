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
      var recentDbs = $.futon.storage.get("recent", "").split(",");
      if ($.inArray(name, recentDbs) == -1) {
        recentDbs.unshift(name);
        if (recentDbs.length > 10) recentDbs.length = 10;
        $.futon.storage.set("recent", recentDbs.join(","));
        this.updateDatabases();
      }
    }

    this.removeDatabase = function(name) {
      // remove database from recent databases list
      var recentDbs = $.futon.storage.get("recent").split(",");
      var recentIdx = $.inArray(name, recentDbs);
      if (recentIdx >= 0) {
        recentDbs.splice(recentIdx, 1);
        $.futon.storage.set("recent", recentDbs.join(","));
        this.updateDatabases();
      }
    }

    this.updateDatabases = function() {
      var selection = null;
      $("#dbs .selected a").each(function() {
        selection = [this.pathname, this.search];
      });
      $("#dbs").empty();
      var recentDbs = $.futon.storage.get("recent").split(",");
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
      $.futon.storage.set("sidebar", hidden ? "hidden" : "show");
    };
  }

  function Storage() {
    var storage = this;
    this.decls = {};

    this.declare = function(name, options) {
      this.decls[name] = $.extend({}, {
        scope: "window",
        defaultValue: null,
        prefix: ""
      }, options || {});
    }

    this.declareWithPrefix = function(prefix, decls) {
      for (var name in decls) {
        var options = decls[name];
        options.prefix = prefix;
        storage.declare(name, options);
      }
    }

    this.del = function(name) {
      lookup(name, function(decl) {
        handlers[decl.scope].del(decl.prefix + name);
      });
    }

    this.get = function(name, defaultValue) {
      return lookup(name, function(decl) {
        var value = handlers[decl.scope].get(decl.prefix + name);
        if (value !== undefined) {
          return value;
        }
        if (defaultValue !== undefined) {
          return defaultValue;
        }
        return decl.defaultValue;
      });
    }

    this.set = function(name, value) {
      lookup(name, function(decl) {
        handlers[decl.scope].set(decl.prefix + name, value);
      });
    }

    function lookup(name, callback) {
      var decl = storage.decls[name];
      if (decl === undefined) {
        return decl;
      }
      return callback(decl);
    }

    var handlers = {

      "cookie": {
        get: function(name) {
          var nameEq = name + "=";
          var parts = document.cookie.split(';');
          for (var i = 0; i < parts.length; i++) {
            var part = parts[i].replace(/^\s+/, "");
            if (part.indexOf(nameEq) == 0) {
              return unescape(part.substring(nameEq.length, part.length));
            }
          }
        },
        set: function(name, value) {
          var date = new Date();
          date.setTime(date.getTime() + 14*24*60*60*1000); // two weeks
          document.cookie = name + "=" + escape(value) + "; expires=" +
            date.toGMTString();
        },
        del: function(name) {
          var date = new Date();
          date.setTime(date.getTime() - 24*60*60*1000); // yesterday
          document.cookie = name + "=; expires=" + date.toGMTString();
        }
      },

      "window": {
        get: function(name) {
          return JSON.parse(window.name || "{}")[name];
        },
        set: function(name, value) {
          var obj = JSON.parse(window.name || "{}");
          obj[name] = value || null;
          window.name = JSON.stringify(obj);
        },
        del: function(name) {
          var obj = JSON.parse(window.name || "{}");
          delete obj[name];
          window.name = JSON.stringify(obj);
        }
      }

    };

  }

  $.futon = $.futon || {};
  $.extend($.futon, {
    navigation: new Navigation(),
    storage: new Storage()
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

  $.fn.enableTabInsertion = function(chars) {
    chars = chars || "\t";
    var width = chars.length;
    return this.keydown(function(evt) {
      if (evt.keyCode == 9) {
        var v = this.value;
        var start = this.selectionStart;
        var scrollTop = this.scrollTop;
        if (start !== undefined) {
          this.value = v.slice(0, start) + chars + v.slice(start);
          this.selectionStart = this.selectionEnd = start + width;
        } else {
          document.selection.createRange().text = chars;
          this.caretPos += width;
        }
        return false;
      }
    });
  }

  $(document)
    .ajaxStart(function() { $(this.body).addClass("loading"); })
    .ajaxStop(function() { $(this.body).removeClass("loading"); });

  $.futon.storage.declare("sidebar", {scope: "cookie", defaultValue: "show"});
  $.futon.storage.declare("recent", {scope: "cookie", defaultValue: ""});

  $(function() {
    document.title = "Apache CouchDB - Futon: " + document.title;
    if ($.futon.storage.get("sidebar") == "hidden") {
      // doing this as early as possible prevents flickering
      $(document.body).addClass("fullwidth");
    }
    $.get("_sidebar.html", function(resp) {
      $("#wrap").append(resp)
        .find("#sidebar-toggle").click(function(e) {
            $.futon.navigation.toggle(e.shiftKey ? 2500 : 500);
            return false;
          });
      if ($.futon.storage.get("sidebar") == "hidden") {
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
