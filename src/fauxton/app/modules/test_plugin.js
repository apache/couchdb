require([
  "modules/fauxton_plugin"
],

function(Plugin) {
  var TestPlugin = Plugin.module();
  var link = {"href": "/foo", "title": "Foo"};

  console.log("Adding header link!");
  console.log(Plugin);
  Plugin.addHeaderLink(link);

  Plugin.addRoute({name:"foo",route:"foo", callback: function() {console.log("ROUTING FOO"); }});

});
