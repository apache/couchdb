#!/usr/bin/env node
// Copyright 2011 Iris Couch
//
//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at
//
//        http://www.apache.org/licenses/LICENSE-2.0
//
//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.

module.exports = main

var os = require('os')
var fs = require('fs')
var URL = require('url')
var util = require('util')
var http = require('http')
var async = require('async')
var mkdirp = require('mkdirp')
var request = require('request')
var optimist = require('optimist')
var pushover = require('pushover')
var child_process = require('child_process')

var console = require('./console')
var VER = require('./package.json').version

var couch = { 'log': mk_couch_log('info')
            , 'warn' : mk_couch_log('warn')
            , 'error': mk_couch_log('error')
            , 'debug': mk_couch_log('debug')
            }


var opts = optimist.usage('$0')

var COUCH = null
var GIT_DIR = null
var COUCH_PASSWORD = null
var GIT_PORT = null
var APPLICATION = null

function main() {
  if(opts.argv.help)
    return console.log(opts.help())

  console.log('Extra CouchDB daemon: %s', process.pid)
  couch.debug('CouchDB daemon %s: %s', VER, process.pid)

  var env = {}
  for (var k in process.env) {
    var match = k.match(/^_couchdb_app_(.*)$/)
    if(match)
      env[match[1]] = process.env[k]
  }

  for (k in env)
    couch.debug('  %s = %s', k, env[k])

  if(env.port && env.password && env.couch && env.dir)
    return git(env)

  setInterval(function() {
    console.log('Still here')
    couch.log('Still in couch')
  }, 60000)
}

function git(env) {
  GIT_PORT = +env.port
  COUCH = env.couch
  COUCH_DIR = util.format('%s/couchjs-%s', env.dir, VER)
  COUCH_PASSWORD = env.password

  //var couch_url = util.format('http://_nodejs:%s@127.0.0.1:%d', password, couch_port)
  //couch.log('couch url %j', couch_url)

  auth('_nodejs', COUCH_PASSWORD, function(er, userCtx) {
    if(er)
      throw er

    var roles = userCtx.roles || []
    if(userCtx.name != '_nodejs' || !~roles.indexOf('_admin'))
      throw new Error('Not admin: ' + JSON.stringify(res.body.userCtx))

    var repos = pushover(COUCH_DIR)
    repos.on('push', function(push) {
      couch.log('Push %s/%s: %s', push.repo, push.commit, push.branch)
      push.accept()
      //couch.log('Response: %j', Object.keys(push.response))
      push.response.on('finish', function() {
        //couch.log('Finished!')
        publish(push)
      })
    })

    repos.on('fetch', function(fetch) {
      couch.log('fetch %j', fetch.commit)
      fetch.accept()
    })

    var server = http.createServer(function(req, res) {
      if(! req.url.match(/^\/_nodejs\/_git(\/|$)/))
        return handle_http(req, res)

      req.pause()
      auth_req(req, function(er, userCtx) {
        if(er && er.statusCode) {
          res.writeHead(er.statusCode, er.headers)
          return res.end(er.body)
        }

        if(er) {
          couch.log('Bad req %s: %s', req.url, er.message)
          return res.end()
        }

        var roles = userCtx.roles || []
        if(!~ roles.indexOf('_admin')) {
          couch.log('Not admin: %s, %j', req.url, userCtx)
          res.writeHead(401, 'Unauthorized', {'content-type':'application/json'})
          return res.end('{"error":"not_authorized"}\n')
        }

        //couch.log('Handle Git: %j', req.url)
        repos.handle(req, res)
        req.resume()
      })
    })

    server.listen(GIT_PORT)
  })
}

function handle_http(req, res) {
  if(! APPLICATION) {
    var headers = { 'content-type': 'application/json'
                  , 'server': 'NodeJS-CouchDB/'+VER
                  }
    res.writeHead(200, 'OK', headers)
    var body = {'ok':true}
    return res.end(JSON.stringify(body) + '\n')
  }

  // Clean up the vhost changes.
  var vhost_path = req.headers['x-couchdb-vhost-path']
  if(vhost_path) {
    req.url = vhost_path
    delete req.headers['x-couchdb-vhost-path']
  }

  APPLICATION(req, res)
}

function auth(user, pass, callback) {
  if(!COUCH)
    return process.nextTick(function() { callback(new Error('No _couchdb_port')) })

  var url = COUCH + '/_session'
  if(user || pass) {
    url = URL.parse(url)
    url.auth = util.format('%s:%s', user || '', pass || '')
    url = URL.format(url)
  }

  //couch.log('auth: %j', url)
  request({'url':url, 'json':true}, function(er, res) {
    //couch.log('auth result: %j', res.body)
    if(er)
      return callback(er)

    if(res.statusCode != 200) {
      er = new Error('Bad status '+res.statusCode+' for auth: ' + res.body)
      er.statusCode = res.statusCode
      er.body = JSON.stringify(res.body) + '\n'
      er.headers = res.headers
      return callback(er)
    }

    return callback(null, res.body.userCtx)
  })
}

function auth_req(req, callback) {
  var headers = req.headers || {}
  var auth_str = req.headers.authorization || ''

  var match = auth_str.match(/^Basic (.+)$/)
  if(!match)
    return auth(null, null, callback)

  try {
    auth_str = new Buffer(match[1], 'base64').toString()
    match = auth_str.match(/^([^:]+):(.+)$/)
  } catch (er) {
    return callback(er)
  }

  if(!match)
    return callback(new Error('Bad auth string: ' + auth_str))

  auth(match[1], match[2], callback)
}


function publish(push) {
  var script = __dirname + '/checkout.sh'
  var repo = COUCH_DIR + '/' + push.repo

  var id = Math.floor(Math.random() * 1000 * 1000)
  var work = util.format('%s/%s/%s', COUCH_DIR, push.commit, id)

  mkdirp(work, function(er) {
    if(er) {
      couch.error('Failed to make working dir: %s', work)
      throw er
    }

    checkout()
  })

  function checkout() {
    var args = [script, repo, push.commit]
    var opts = { 'cwd':work, 'stdio':'pipe' }

    var child = child_process.spawn('bash', args, opts)

    var output = []

    child.stdout.on('data', function(x) {
      var msg = util.format('OUT %s', x.toString().trim())
      couch.debug(msg)
      output.push(msg)
    })
    child.stderr.on('data', function(x) {
      var msg = util.format('ERR %s', x.toString().trim())
      couch.debug(msg)
      output.push(msg)
    })

    child.on('exit', function(code) {
      if(code !== 0) {
        couch.error('Bad checkout: %d', code)
        output.forEach(function(line) {
          couch.error(line)
        })

        throw new Error('Bad checkout')
      }

      couch.log('Checked out push: %s', work)
      fs.readFile(work+'/package.json', 'utf8', function(er, body) {
        if(er)
          throw er

        body = JSON.parse(body)
        if(!body.couchdb)
          return couch.warn('No "couchdb" value in pushed package.json')

        run_app(work, body)
      })
    })
  }
}


function run_app(work_dir, pkg) {
  var vhosts = []
    , main = null

  if(typeof pkg.couchdb == 'string')
    main = pkg.couchdb
  else {
    vhosts = pkg.couchdb.vhosts || []
    main = pkg.couchdb.main
  }

  couch.log('Run app %s: %j', main, vhosts)

  var mod_path = util.format('%s/%s', work_dir, main)
  try {
    var ok = require.resolve(mod_path)
  } catch (er) {
    return couch.error('Bad module path: %s', mod_path)
  }

  couch_mod = require(mod_path)
  APPLICATION = couch_mod
  couch.log('Installed CouchDB application')

  return async.forEach(vhosts, set_vhost, vhosts_set)

  function set_vhost(vhost, to_async) {
    var couch_url = URL.parse(COUCH)
    couch_url.auth = '_nodejs:' + COUCH_PASSWORD
    couch_url = URL.format(couch_url)
    couch.log('couch_url: %j', couch_url)

    var url = couch_url + '_config/vhosts/' + vhost
    var body = '/_nodejs'
    request.put({'url':url, 'json':body}, function(er, res) {
      if(er)
        return to_async(er)
      if(res.statusCode != 200)
        return to_async(new Error('Bad response '+res.statusCode+' to vhost: ' + vhost))

      couch.log('Set vhost: %s', vhost)
      return to_async()
    })
  }

  function vhosts_set(er) {
    if(er)
      throw er

    couch.log('Set %d vhosts for CouchDB application', vhosts.length)
  }
}


//
// Utilities
//

function mk_couch_log(level) {
  if(level == 'warn')
    level = 'error'

  return logger

  function logger() {
    var str = util.format.apply(util, arguments)
    var msg = ['log', str, {'level':level}]
    msg = JSON.stringify(msg)
    process.stdout.write(msg + '\n')
  }
}


if(require.main === module)
  main()
