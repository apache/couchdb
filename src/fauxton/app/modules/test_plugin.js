require([
  "fauxton_api"
],

function(FauxtonAPI) {
  var TestPlugin = FauxtonAPI.module();
  var link = {"href": "/foo", "title": "Foo"};

  console.log("Adding header link!");
  console.log(FauxtonAPI);
  FauxtonAPI.addHeaderLink(link);

  FauxtonAPI.addRoute({name:"foo",route:"foo", callback: function() {console.log("ROUTING FOO"); }});

});
