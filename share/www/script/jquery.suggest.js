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

  suggest = function(elem, options) {
    var timer = null;
    var prevVal = null;

    var input = $(elem).attr("autocomplete", "off");
    var offset = input.offset();
    var dropdown = $('<ul style="z-index: 10000"></ul>')
      .addClass(options.dropdownClass).appendTo("body").css({
        top: (offset.top + elem.offsetHeight) + "px",
        left: offset.left + "px",
        minWidth: input.css("width")
      });

    input
      .blur(function() {
        setTimeout(function() { dropdown.hide() }, 200);
      })
      .keydown(function(e) {
        if (timer) clearTimeout(timer);
        if ($.inArray(e.keyCode, [38, 40]) != -1 ||
            (dropdown.is(":visible") && (e.keyCode == 27 ||
             ($.inArray(e.keyCode, [9, 13]) != -1 && getSelection())))) {
          e.preventDefault(); e.stopPropagation();
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
              break;
            case 27: // escape
              dropdown.hide();
              break;
          }
          return false;
        } else {
          timer = setTimeout(function() { suggest() }, options.delay);
        }
      });

    function suggest(force) {
      var newVal = $.trim(input.val());
      if (force || newVal != prevVal) {
        if (force || newVal.length >= options.minChars) {
          options.callback($.trim(input.val()), function(items) {
            show(items);
          });
        } else {
          dropdown.hide();
        }
        prevVal = newVal;
      }
    }

    function show(items) {
      if (!items) return;
      if (!items.length) { dropdown.hide(); return; }
      var html = [];
      for (var i = 0; i < items.length; i++) {
        html.push('<li>' + items[i] + '</li>');
      }
      dropdown.html(html.join("")).slideDown("fast");
      dropdown.children('li').click(function(e) {
        $(this).addClass("selected");
        commit();
      });
    }

    function commit() {
      var sel = getSelection();
      if (sel) {
        prevVal = sel.text();
        input.val(prevVal);
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
    options = options || {};
    options.callback = callback;
    options.delay = options.delay || 100;
    options.dropdownClass = options.dropdownClass || "suggest-dropdown";
    options.minChars = options.minChars || 1;
    return this.each(function() {
      suggest(this, options);
    });
  };

})(jQuery);
