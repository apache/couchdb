define([
],

function() {

  var Resize = function(options){
    this.options = options;
    this.options.selectorElements = options.selectorElements || "#dashboard-content";
  };

  Resize.prototype = {
    getPrimaryNavWidth: function(){
      var primaryNavWidth  = $('body').hasClass('closeMenu')? 66:220;
      return primaryNavWidth;
    },
    getPanelWidth: function(){
      var sidebarWidth = $('#sidebar-content').length > 0 ? $('#sidebar-content').width(): 0;
      return (this.getPrimaryNavWidth() + sidebarWidth); 
    },
    initialize: function(){
      $(window).off('resize');
      var that = this;
      //add throttler :) 
      this.lazyLayout = _.debounce(that.onResizeHandler, 300).bind(this);
      $(window).on('resize', this.lazyLayout);
      this.onResizeHandler();
    },
    updateOptions:function(options){
      this.options = {};
      this.options = options;
    },
    turnOff:function(){
      $(window).off('resize');
    },
    onResizeHandler: function (){
      //if there is an override, do that instead
      console.log("onResizeHandler");
      if (this.options.onResizeHandler){
        this.options.onResizeHandler();
      } else {
        var combinedWidth = window.innerWidth - this.getPanelWidth(),
        smallWidthConstraint = ($('#sidebar-content').length > 0)? 470:800,
        panelWidth; 

        if( combinedWidth > smallWidthConstraint  && combinedWidth < 1400){
          panelWidth = window.innerWidth - this.getPanelWidth();
        } else if (combinedWidth < smallWidthConstraint){
          panelWidth = smallWidthConstraint;
        } else if(combinedWidth > 1400){
          panelWidth = 1400;
        }

        $(this.options.selectorElements).innerWidth(panelWidth);
      }
      //if there is a callback, run that
      if(this.options.callback) {
        this.options.callback();
      }
    } 
  };

  return Resize;
});
