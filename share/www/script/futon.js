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

// $$ inspired by @wycats: http://yehudakatz.com/2009/04/20/evented-programming-with-jquery/
function $$(node) {
  var data = $(node).data("$$");
  if (data) {
    return data;
  } else {
    data = {};
    $(node).data("$$", data);
    return data;
  }
};

(function($) {

  function Session() {

    function doLogin(name, password, callback) {
      $.couch.login({
        name : name,
        password : password,
        success : function() {
          $.futon.session.sidebar();
          callback();
        },
        error : function(code, error, reason) {
          $.futon.session.sidebar();
          callback({name : "Error logging in: "+reason});
        }
      });
    };

    function doSignup(name, password, callback, runLogin) {
      $.couch.signup({
        name : name
      }, password, {
        success : function() {
          if (runLogin) {
            doLogin(name, password, callback);
          } else {
            callback();
          }
        },
        error : function(status, error, reason) {
          $.futon.session.sidebar();
          if (error == "conflict") {
            callback({name : "Name '"+name+"' is taken"});
          } else {
            callback({name : "Signup error:  "+reason});
          }
        }
      });
    };

    function validateUsernameAndPassword(data, callback) {
      if (!data.name || data.name.length == 0) {
        callback({name: "Please enter a name."});
        return false;
      };
      return validatePassword(data, callback);
    };

    function validatePassword(data, callback) {
      if (!data.password || data.password.length == 0) {
        callback({password: "Please enter a password."});
        return false;
      };
      return true;
    };

    function createAdmin() {
      $.showDialog("dialog/_create_admin.html", {
        submit: function(data, callback) {
          if (!validateUsernameAndPassword(data, callback)) return;
          $.couch.config({
            success : function() {
              setTimeout(function() {
                doLogin(data.name, data.password, function(errors) {
                  if(!$.isEmptyObject(errors)) {
                    callback(errors);
                    return;
                  }
                  doSignup(data.name, null, function(errors) {
                    if (errors && errors.name && errors.name.indexOf && errors.name.indexOf("taken") == -1) {
                      callback(errors);
                    } else {
                      callback();
                    }
                    }, false);
                  });
              }, 200);
            }
          }, "admins", data.name, data.password);
        }
      });
      return false;
    };

    function login() {
      $.showDialog("dialog/_login.html", {
        submit: function(data, callback) {
          if (!validateUsernameAndPassword(data, callback)) return;
          doLogin(data.name, data.password, callback);
        }
      });
      return false;
    };

    function logout() {
      $.couch.logout({
        success : function(resp) {
          $.futon.session.sidebar();
        },
        error: function(status, e, reason) {
          alert('An error occurred logging out: ' + reason);
        }
      })
    };

    function signup() {
      $.showDialog("dialog/_signup.html", {
        submit: function(data, callback) {
          if (!validateUsernameAndPassword(data, callback)) return;
          doSignup(data.name, data.password, callback, true);
        }
      });
      return false;
    };

    function changePassword () {
      var updateUserDoc = function(resp, data) {
        // regular users get their _users doc updated
        $.couch.db(resp.info.authentication_db).openDoc("org.couchdb.user:"+resp.userCtx.name, {
          error: function () {
            // ignore 404
            location.reload();
          },
          success: function (user) {
            user.password = data.password;
            $.couch.db(resp.info.authentication_db).saveDoc(user, {
              success: function() {
                doLogin(user.name, user.password, function(errors) {
                    if(!$.isEmptyObject(errors)) {
                      callback(errors);
                      return;
                    } else {
                      location.reload();
                    }
                  });
                }
              });
            }
        });
      }

      $.showDialog("dialog/_change_password.html", {
        submit: function(data, callback) {
          if (validatePassword(data, callback)) {
            if (data.password != data.verify_password) {
              callback({verify_password: "Passwords don't match."});
              return false;
            }
          } else {
            return false;
          }
          $.couch.session({
            error: function(status, e, reason) {
              alert('Could not get your session info: ' + reason);
            },
            success: function (resp) {
              // admin users may have a config entry, change the password
              // there first. Update their user doc later, if it exists
              if (resp.userCtx.roles.indexOf("_admin") > -1) { // user is admin
                // check whether we have a config entry
                $.couch.config({
                  success : function (response) { // er do have a config entry
                    $.couch.config({
                      success : function () {
                        window.setTimeout(function() {
                          doLogin(resp.userCtx.name, data.password, function(errors) {
                            if(!$.isEmptyObject(errors)) {
                              callback(errors);
                              return;
                            } else {
                              location.reload();
                            }
                          });
                        }, 1000);
                      },
                      error: function(status, e, reason) {
                        callback('Could not persist the new password: ' + reason);
                      }
                    }, "admins", resp.userCtx.name, data.password);
                  }
                }, "admins", resp.userCtx.name);
              } else { // non-admin users, update their user doc
                updateUserDoc(resp, data);
              }
            }
          });
        }
      });
      return false;
    };

    this.setupSidebar = function() {
      $("#userCtx .login").click(login);
      $("#userCtx .logout").click(logout);
      $("#userCtx .signup").click(signup);
      $("#userCtx .createadmin").click(createAdmin);
      $("#userCtx .changepass").click(changePassword);
    };

    this.sidebar = function() {
      // get users db info?
      $("#userCtx span").hide();
      $(".serverAdmin").attr('disabled', 'disabled');

      $.couch.session({
        success : function(r) {
          var userCtx = r.userCtx;

          var urlParts = location.search.substr(1).split("/");
          var dbName = decodeURIComponent(urlParts.shift());
          var dbNameRegExp = new RegExp("[^a-z0-9\_\$\(\)\+\/\-]", "g");
          dbName = dbName.replace(dbNameRegExp, "");

          $$("#userCtx").userCtx = userCtx;
          if (userCtx.name) {
            $("#userCtx .name").text(userCtx.name).attr({href : $.couch.urlPrefix + "/_utils/document.html?"+encodeURIComponent(r.info.authentication_db)+"/org.couchdb.user%3A"+encodeURIComponent(userCtx.name)});

            if (userCtx.roles.indexOf("_admin") != -1) {
              $("#userCtx .loggedin").show();
              $("#userCtx .loggedinadmin").show();
              $(".serverAdmin").removeAttr('disabled'); // user is a server admin
            } else {
              $("#userCtx .loggedin").show();

              if (dbName != "") {
                $.couch.db(dbName).getDbProperty("_security", { // check security roles for user admins
                  success: function(resp) {
                    var adminRoles = resp.admins.roles;

                    if ($.inArray(userCtx.name, resp.admins.names)>=0) { // user is admin
                      $(".userAdmin").removeAttr('disabled');
                    }
                    else {
                      for (var i=0; i<userCtx.roles.length; i++) { 
                        if ($.inArray(userCtx.roles[i], resp.admins.roles)>=0) { // user has role that is an admin
                          $(".userAdmin").removeAttr('disabled');
                        }
                      }
                    }
                  } 
                }); 
              }
            }
          } else if (userCtx.roles.indexOf("_admin") != -1) {
            $("#userCtx .adminparty").show();
            $(".serverAdmin").removeAttr('disabled');
          } else {
            $("#userCtx .loggedout").show();
          };
        }
      })
    };
  };

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
      var current = $.futon.storage.get("recent", "");
      var recentDbs = current ? current.split(",") : [];
      if ($.inArray(name, recentDbs) == -1) {
        recentDbs.unshift(name);
        if (recentDbs.length > 10) recentDbs.length = 10;
        $.futon.storage.set("recent", recentDbs.join(","));
        this.updateDatabases();
      }
    }

    this.removeDatabase = function(name) {
      // remove database from recent databases list
      var current = $.futon.storage.get("recent", "");
      var recentDbs = current ? current.split(",") : [];
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
          name = encodeURIComponent(name);
          $("#dbs").append("<li>" +
            "<button class='remove' title='Remove from list' value='" + name + "'></button>" +
            "<a href='database.html?" + name + "' title='" + name + "'>" + name +
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
        if (value == decl.defaultValue) {
          handlers[decl.scope].del(decl.prefix + name);
        } else {
          handlers[decl.scope].set(decl.prefix + name, value);
        }
      });
    }

    function lookup(name, callback) {
      var decl = storage.decls[name];
      if (decl === undefined) {
        return decl;
      }
      return callback(decl);
    }

    function windowName() {
      try {
        return JSON.parse(window.name || "{}");
      } catch (e) {
        return {};
      }
    }

    // add suffix to cookie names to be able to separate between ports
    var cookiePrefix = location.port + "_";

    var handlers = {

      "cookie": {
        get: function(name) {
          var nameEq = cookiePrefix + name + "=";
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
          document.cookie = cookiePrefix + name + "=" + escape(value) +
            "; expires=" + date.toGMTString();
        },
        del: function(name) {
          var date = new Date();
          date.setTime(date.getTime() - 24*60*60*1000); // yesterday
          document.cookie = cookiePrefix + name + "=" +
            "; expires=" + date.toGMTString();
        }
      },

      "window": {
        get: function(name) {
          return windowName()[name];
        },
        set: function(name, value) {
          var obj = windowName();
          obj[name] = value || null;
          window.name = JSON.stringify(obj);
        },
        del: function(name) {
          var obj = windowName();
          delete obj[name];
          window.name = JSON.stringify(obj);
        }
      }

    };

  }

  $.couch.urlPrefix = "..";
  $.futon = $.futon || {};
  $.extend($.futon, {
    navigation: new Navigation(),
    session : new Session(),
    storage: new Storage()
  });

  $.fn.addPlaceholder = function() {
    if (this[0] && "placeholder" in document.createElement("input")) {
      return; // found native placeholder support
    }
    return this.live('focusin', function() {
      var input = $(this);
      if (input.val() === input.attr("placeholder")) {
        input.removeClass("placeholder").val("");
      }
    }).live("focusout", function() {
      var input = $(this);
      if (input.val() === "") {
        input.val(input.attr("placeholder")).addClass("placeholder");
      }
    }).trigger("focusout");
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
    $("input[placeholder]").addPlaceholder();

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
      $.futon.session.setupSidebar();
      $.futon.session.sidebar();

      $.couch.info({
        success: function(info, status) {
          $("#version").text(info.version);
        }
      });
    });
  });

})(jQuery);
