// from https://github.com/KlausTrainer/sandbox.js
exports.runInSandbox = function(src, ctx, whitelist) {
  var vm = require('vm');
  var sandbox;

  if (ctx && ctx.require) {

    whitelist = whitelist || [];

    var insecureRequire = ctx.require;
    var module = require('module');
    var oldModulePrototype = module.prototype;

    var secureRequire = function(moduleName) {

      if (whitelist.indexOf(moduleName) === -1) {
        module.prototype = oldModulePrototype;

        throw new Error("'" + moduleName + "' is not whitelisted");

      } else {
        var requiredModule = insecureRequire(moduleName);

        module.prototype = oldModulePrototype;

        return requiredModule;
      }
    };

    module.prototype = {
      require: secureRequire,
      load: module.prototype.load,
      _compile: module.prototype._compile
    };

    module._cache = {};

    ctx.require = secureRequire;
    sandbox = Object.freeze(vm.createContext(ctx));
    ctx.require = insecureRequire;

  } else {
    sandbox = Object.freeze(vm.createContext(ctx || {}));
  }

  return vm.createScript('(function() {"use strict"; return (' + src + ')()}())').runInContext(sandbox);
};
