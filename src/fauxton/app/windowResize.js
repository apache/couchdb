define([
],

function() {

  var Resize = function(options){
    this.options = options;
    this.options.selectorElements = options.selectorElements || "#dashboard-content";
  };

  Resize.prototype = {
    getPanelWidth: function(){
      var sidebarWidth = $('#sidebar-content').length > 0?$('#sidebar-content').width(): 40,
          primeNavWidth = parseInt($('#dashboard').css('left').replace('px',''),10);
      return (primeNavWidth +sidebarWidth); 
    },
    initialize: function(){
      $(window).off('resize');
      var that = this;
      //add throttler :) 
      this.lazyLayout = _.debounce(that.onResizeHandler, 300).bind(this);
      $(window).on('resize', this.lazyLayout);
      this.onResizeHandler();
    },
    onResizeHandler: function (){
      //if there is an override, do that instead
      if (this.options.onResizeHandler){
        this.options.onResizeHandler();
      } else {
        var panelWidth = (window.innerWidth - this.getPanelWidth() < 1100) ? window.innerWidth - this.getPanelWidth() :1100;
        $(this.options.selectorElements).width(panelWidth);
      }
      //if there is a callback, run that
      if(this.options.callback) {
        this.options.callback();
      }
    } 
  };

  return Resize;
});
