Heya,

I couldn’t help myself thinking about plugin stuff and ended up
whipping up a proof of concept.

Here’s a <1 minute demo video:

  https://dl.dropboxusercontent.com/u/82149/couchdb-plugins-demo.mov

Alternative encoding:

  https://dl.dropboxusercontent.com/u/82149/couchdb-plugins-demo.m4v)


In my head the whole plugin idea is a very wide area, but I was so
intrigued by the idea of getting something running with a click on a
button in Futon. So I looked for a minimally viable plugin system.


## Design principles

It took me a day to put this all together and this was only possible
because I took a lot of shortcuts. I believe they are all viable for a
first iteration of a plugins system:

1. Install with one click on a button in Futon (or HTTP call)
2. Only pure Erlang plugins are allowed.
3. The plugin author must provide a binary package for each Erlang (and,
   later, each CouchDB version).
4. Complete trust-based system. You trust me to not do any nasty things
   when you click on the install button. No crypto, no nothing. Only
   people who can commit to Futon can release new versions of plugins.
5. Minimal user-friendlyness: won’t install plugins that don’t match 
   the current Erlang version, gives semi-sensible error messages
   (wrapped in a HTTP 500 response :)
6. Require a pretty strict format for binary releases.


## Roadmap

Here’s a list of things this first iterations does and doesn’t do:

- Pure Erlang plugins only. No C-dependencies, no JavaScript, no nothing.
- No C-dependencies.
- Install a plugin via Futon (or HTTP call). Admin only.
- A hardcoded list of plugins in Futon.
- Loads a pre-packaged, pre-compiled .tar.gz file from a URL.
- Only installs if Erlang version matches.
- No security checking of binaries.
- No identity checking of binaries.

Here are a few things I want to add before I call it MVP*:

- Uninstall a plugin via Futon (or HTTP call). Admin only.
- Only installs if CouchDB version matches.
- Binaries must be published on *.apache.org.
- Register installed plugins in the config system.
- Make sure plugins start with the next restart of CouchDB.
- Show when a particular plugin is installed.

*MVP hopefully means you agree we can ship this with a few warnings
so people can get a hang of it.

Here is a rough list of features squared against future milestones:

Milestone 2: Be creator friendly
 - Make it easy to build a CouchDB plugin by providing one or more easy 
   to start templates.
 - Make it easy to publish new plugins and new versions of existing plugins.
 - Make it easy to supply packages for multiple Erlang & CouchDB versions.

Milestone 3: Public registry
 - Instead of hardcoding a list of plugins into Futon/Fauxton, we load
   a list of applicable plugins from a central (and configurable)
   plugins repository.
 - This allows plugin authors to publish new plugins and new versions
   of existing plugins independently.

Milestone 4: Other Languages
 - Figure out how to handle C-dependencies for Erlang plugins.
 - Figure out how to allow other language plugins
   (c.f. non-JS query servers)

Milestone X: Later
 - Add some account/identity/maybe crypto-web-of-trust system for
   authors to publish “legit” plugins.
 - Sign & verify individual releases.

A few more things that can happen concurrently depending on what
plugins require:
 - Integrate Erlang/JS tests in the installation
 - Integrate docs


## How it works

This plugin system lives in `src/couch_plugins` and is a tiny CouchDB
module.

It exposes one new API endpoint `/_plugins` that an admin user can
POST to.

The additional Futon page lives at /_utils/plugins.html it is
hardcoded.

Futon (or you) post an object to `/_plugins` with four properties:

    {
      "name": "geocouch", // name of the plugin, must be unique
      "url": "http://people.apache.org/~jan", // “base URL” for plugin releases (see below)
      "version": "couchdb1.2.x_v0.3.0-11-gd83ba22", // whatever version internal to the plugin
      "checksums": {
        "R15B03": "ZetgdHj2bY2w37buulWVf3USOZs=" // base64’d sha hash over the binary
      }
    }

`couch_plugins` then attempts to download a .tar.gz from this
location:

    http://people.apache.org/~jan/geocouch-couchdb1.2.x_v0.3.0-12-g4ea0bea-R15B03.tar.gz

It should be obvious how the URL is constructed from the POST data.
(This url is live, feel free to play around with this tarball).

Next it calculates the sha hash for the downloaded .tar.gz file and
matches it against the correct version in the `checksums` parameter.

If that succeeds, we unpack the .tar.gz file (currently in `/tmp`,
need to find a better place for this) and adds the extracted directory
to the Erlang code path
(`code:add_path("/tmp/couchdb_plugins/geocouch-couchdb1.2.x_v0.3.0-12-g4ea0bea-R15B03/ebin")`)
and loads the included application (`application:load(geocouch)`).

Then it looks into the `./config` directory that lives next to `ebin/`
in the plugin directory for a file `config.erlt` (“erl-terms”). with a
list of configuration parameters to load. We parse the file and set
the config directives one by one.

If that all goes to plan, we report success back to the HTTP caller.

That’s it! :)

It’s deceptively simple, probably does a few things very wrong and
leaves a few things open (see above).

One open question I’d like an answer for is finding a good location to
unpack & install the plugin files that isn’t `tmp`. If the answer is
different for a pre-BigCouch/rcouch-merge and post-BigCouch/rcouch-
merge world, I’d love to know :)


## Code

The main branch for this is 1867-feature-plugins:

     ASF: https://git-wip-us.apache.org/repos/asf?p=couchdb.git;a=log;h=refs/heads/1867-feature-plugins
  GitHub: https://github.com/janl/couchdb/compare/1867-feature-plugins

I created a branch on GeoCouch that adds a few lines to its `Makefile`
that shows how a binary package is built:

    https://github.com/janl/geocouch/compare/couchbase:couchdb1.3.x...couchdb1.3.x-plugins

* * *

I hope you like this :) Please comment and improve heavily!

Let me know if you have any questions :)

If you have any criticism, please phrase it in a way that we can use
to improve this, thanks!

Best,
Jan
-- 
