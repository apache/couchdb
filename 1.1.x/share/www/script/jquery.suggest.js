// http://svn.apache.org/repos/asf/couchdb/trunk/share/www/script/jquery.suggest.js

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

  function suggest(elem, options) {
    var timer = null;
    var prevVal = null;
    var cache = {};
    var cacheKeys = [];

    var input = $(elem).attr("autocomplete", "off");
    var dropdown = $('<ul style="display: none; position: absolute; z-index: 10000"></ul>')
      .addClass(options.dropdownClass).appendTo(document.body);

    input
      .blur(function() {
        if (timer) clearTimeout(timer);
        setTimeout(function() { dropdown.hide() }, 200);
      })
      .keydown(function(e) {
        if ($.inArray(e.keyCode, [16, 17, 18, 20, 144, 91, 93, 224]) != -1) {
          return; // ignore modifier keys
        }
        if (timer) clearTimeout(timer);
        if ($.inArray(e.keyCode, [38, 40]) != -1 ||
            (dropdown.is(":visible") && (e.keyCode == 27 ||
             ($.inArray(e.keyCode, [9, 13]) != -1 && getSelection())))) {
          switch(e.keyCode) {
            case 38: // up
              moveUp();
              break;
            case 40: // down
              moveDown();
              break;
            case 9:  // tab
            case 13: // return
              commit();
              if (e.keyCode == 9) return true;
              break;
            case 27: // escape
              dropdown.hide();
              break;
          }
          e.preventDefault(); e.stopPropagation();
          return false;
        } else {
          timer = setTimeout(function() { suggest() }, options.delay);
        }
      });

    function suggest(force) {
      var newVal = $.trim(input.val());
      if (force || newVal != prevVal) {
        if (force || newVal.length >= options.minChars) {
          if (options.cache && cache.hasOwnProperty(newVal)) {
            show(cache[newVal].items, cache[newVal].render);
          } else {
            options.callback.apply(elem, [newVal, function(items, render) {
              if (options.cache) {
                if (cacheKeys.length >= options.cacheLimit) {
                  delete cache[cacheKeys.shift()];
                }
                cache[newVal] = {items: items, render: render};
                cacheKeys.push(newVal);
              }
              show(items, render);
            }]);
          }
        } else {
          dropdown.hide();
        }
        prevVal = newVal;
      }
    }

    function show(items, render) {
      if (!items) return;
      if (!items.length) { dropdown.hide(); return; }
      var offset = input.offset();
      dropdown.empty().css({
        top: (offset.top + input.outerHeight()) + "px", left: offset.left + "px",
        minWidth: input.css("width")
      });
      render = render || function(idx, value) { return value; }
      for (var i = 0; i < items.length; i++) {
        var item = $("<li></li>").data("value", items[i]);
        var rendered = render(i, items[i]);
        if (typeof(rendered) == "string") {
          item.text(rendered);
        } else {
          item.append(rendered);
        }
        item.appendTo(dropdown);
      }
      dropdown.slideDown("fast");
      dropdown.children("li").click(function(e) {
        $(this).addClass("selected");
        commit();
      });
    }

    function commit() {
      var sel = getSelection();
      if (sel) {
        prevVal = sel.data("value");
        input.val(prevVal);
        if (options.select) {
          options.select.apply(elem, [prevVal]);
        }
        dropdown.hide();
      }
      if (timer) clearTimeout(timer)
    }

    function getSelection() {
      if (!dropdown.is(":visible")) return null;
      var sel = dropdown.children("li.selected");
      return sel.length ? sel : null;
    }

    function moveDown() {
      if (!dropdown.is(":visible")) suggest(true);
      var sel = getSelection();
      if (sel) sel.removeClass("selected").next().addClass("selected");
      else dropdown.children("li:first-child").addClass("selected");
    }

    function moveUp() {
      if (!dropdown.is(":visible")) suggest(true);
      var sel = getSelection();
      if (sel) sel.removeClass("selected").prev().addClass("selected");
      else dropdown.children("li:last-child").addClass("selected");
    }
  }

  $.fn.suggest = function(callback, options) {
    options = $.extend({
      cache: true,
      cacheLimit: 10,
      callback: callback,
      delay: 250,
      dropdownClass: "suggest-dropdown",
      minChars: 1,
      select: null
    }, options || {});
    return this.each(function() {
      suggest(this, options);
    });
  };

})(jQuery);
