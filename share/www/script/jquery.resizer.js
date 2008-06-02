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

  $.fn.makeResizable = function(options) {
    options = options || {};
    options.always = options.always || false;
    options.grippie = options.grippie || null;
    options.minWidth = options.minWidth || 100;
    options.maxWidth = options.maxWidth || null;
    options.minHeight = options.minHeight || 32;
    options.maxHeight = options.maxHeight || null;

    if (!options.always && $.browser.safari && parseInt($.browser.version) >= 522)
      return this; // safari3 and later provides textarea resizing natively

    return this.each(function() {
      var grippie = options.grippie;
      if (!grippie) grippie = $("<div></div>").appendTo(this.parentNode);
      grippie.addClass("grippie");
      var elem = $(this);
      grippie.mousedown(function(e) {
        var pos = {x: e.screenX, y: e.screenY};
        var dimensions = {width: elem.width(), height: elem.height()};
        $(document)
          .mousemove(function(e) {
            if (options.horizontal) {
              var offset = e.screenX - pos.x;
              if (offset) {
                var newWidth = dimensions.width + offset;
                if (newWidth >= options.minWidth &&
                    (!options.maxWidth || newWidth <= options.maxWidth)) {
                  elem.width(newWidth);
                  dimensions.width = newWidth;
                }
                pos.x = e.screenX;
              }
            }
            if (options.vertical) {
              var offset = e.screenY - pos.y;
              if (offset) {
                var newHeight = dimensions.height + offset;
                if (newHeight >= options.minHeight &&
                    (!options.maxHeight || newHeight <= options.maxHeight)) {
                  elem.height(newHeight);
                  dimensions.height = newHeight;
                }
                pos.y = e.screenY;
              }
            }
            document.onselectstart = function() { return false }; // for IE
            return false;
          })
          .one("mouseup", function() {
            $(document).unbind("mousemove");
            document.onselectstart = null; // for IE
          });
        return false;
      });
    });
  }

})(jQuery);
