const MAX_LINE = 10240

class IO {
    constructor () {
        // this.encoder = new TextEncoder()
    }

    async readline () {
        let buffer = new Uint8Array(MAX_LINE)
        let bytes
        while (bytes = await Deno.read(Deno.stdin.rid, buffer)) {
            let text = new TextDecoder().decode(buffer).substr(0, bytes - 1) // strip newline
            return text
        }
    }

    async print (msg) {
        this.encoder = new TextEncoder()
        await Deno.write(Deno.stdout.rid, this.encoder.encode(msg + '\n'))
        // console.log(msg)
    }

    writeline (obj) {
        try {
            this.print(JSON.stringify(obj))
        } catch (e) {
            this.log("Error converting object to JSON: " + e.toString())
            this.log("error on obj: " + (obj.toSource ? obj.toSource() : obj.toString()))
        }
    }

    log (message) {
        if (typeof message == "xml") {
            message = message.toXMLString();
        } else if (typeof message != "string") {
            message = JSON.stringify(message);
        }
        this.writeline(["log", String(message)]);
    }
}

class State {
    constructor (couch, io) {
        this.couch = couch
        this.io = io
        this.funs = []
        this.lib = null
        this.query_config = {}
    }

    reset (config) {
        // clear the globals and run gc
        this.funs = []
        this.lib = null
        this.query_config = config || {}
        this.io.print("true"); // indicates success
    }

    addFun (newFun) {
        // Compile to a function and add it to funs array
        this.funs.push(this.couch.compileFunction(newFun, {views : {lib : this.lib}}))
        Debug.debug(`this.funs`)
        Debug.debug(this.funs)
        this.io.print("true")
    }

    addLib (lib) { // TODO: take out
        this.lib = lib
        this.io.print("true")
    }
}

class Filter {
    constructor (couch, io) {
        this.couch = couch
        this.io = io
        this.view_emit = false;
    }

    emit (key, value) {
        view_emit = true;
    }

    createFilterSandbox () {
        var sandbox = this.couch.createSandbox();
        sandbox.emit = this.emit;
        return sandbox;
    }

    filter (fun, ddoc, args) {
        var results = [];
        var docs = args[0];
        var req = args[1];
        for (var i=0; i < docs.length; i++) {
            results.push((fun.apply(ddoc, [docs[i], req]) && true) || false);
        };
        this.io.writeline([true, results]);
    }

    filter_view (fun, ddoc, args) {
        // recompile
        var sandbox = this.createFlterSandbox();
        var source = fun.toSource ? fun.toSource() : '(' + fun.toString() + ')';

        const emit = sandbox.emit;
        const log = sandbox.log;
        const JSON = sandbox.JSON;

        fun = eval(source);

        var results = [];
        var docs = args[0];
        for (var i=0; i < docs.length; i++) {
            view_emit = false;
            fun(docs[i]);
            results.push((view_emit && true) || false);
        };
        this.io.writeline([true, results]);
    }
}


class DesignDoc {
    constructor (couch, io) {
        this.couch = couch
        this.io = io
        this.validate = new Validate(io)
        this.filter = new Filter(couch, io)
        this.ddoc_dispatch = {
            "filters": [this.filter, this.filter.filter],
            "views": [this.filter, this.filter.filter_view],
            "validate_doc_update": [this.validate, this.validate.validate]
        }
        this.ddocs = {}
    }

    ddoc () {
        Debug.debug('> ddoc()')
        var args = []
        for (var i=0; i < arguments.length; i++) {
            args.push(arguments[i])
        }
        Debug.debug('ddoc args' + JSON.stringify(args))
        var ddocId = args.shift()
        if (ddocId == "new") {
            // get the real ddocId.
            ddocId = args.shift()
            // store the ddoc, functions are lazily compiled.
            this.ddocs[ddocId] = args.shift()
            Debug.debug('ddocs: ' + JSON.stringify(this.ddocs))
            this.io.print("true")
        } else {
            // Couch makes sure we know this ddoc already.
            Debug.debug('ddocs: known')
            var ddoc = this.ddocs[ddocId]
            if (!ddoc) throw(["fatal", "query_protocol_error", "uncached design doc: "+ddocId])
            var funPath = args.shift()
            var cmd = funPath[0]
            // the first member of the fun path determines the type of operation
            var funArgs = args.shift()
            if (this.ddoc_dispatch[cmd]) {
                // get the function, call the command with it
                var point = ddoc
                for (var i=0; i < funPath.length; i++) {
                    if (i+1 == funPath.length) {
                        var fun = point[funPath[i]]
                        if (!fun) {
                            throw(["error","not_found",
                                "missing " + funPath[0] + " function " + funPath[i] +
                                " on design doc " + ddocId])
                        }
                        if (typeof fun != "function") {
                            fun = this.couch.compileFunction(fun, ddoc, funPath.join('.'))
                            // cache the compiled fun on the ddoc
                            point[funPath[i]] = fun
                        }
                    } else {
                        point = point[funPath[i]]
                    }
                }

                // run the correct responder with the cmd body
                const [object, method] = this.ddoc_dispatch[cmd]
                method.apply(object, [fun, ddoc, funArgs])
            } else {
                // unknown command, quit and hope the restarted version is better
                throw(["fatal", "unknown_command", "unknown ddoc command '" + cmd + "'"])
            }
        }
    }
}

class Views {
    constructor (couch, io, state) {
        this.couch = couch
        this.io = io
        this.map_results = [] // holds temporary emitted values during doc map
        this.state = state
    }

    runReduce (reduceFuns, keys, values, rereduce) {
        var code_size = 0
        for (var i in reduceFuns) {
            var fun_body =  reduceFuns[i]
            code_size += fun_body.length
            reduceFuns[i] = this.couch.compileFunction(fun_body)
        }
        var reductions = new Array(reduceFuns.length)
        for(var i = 0; i < reduceFuns.length; i++) {
            try {
                reductions[i] = reduceFuns[i](keys, values, rereduce)
            } catch (err) {
                Debug.debug(`reduce err`)
                Debug.debug(JSON.stringify(doc))
                handleViewError(err)
                // if the error is not fatal, ignore the results and continue
                reductions[i] = null
            }
        }
        var reduce_line = JSON.stringify(reductions)
        var reduce_length = reduce_line.length
        var input_length =  this.state.line_length - code_size
        // TODO make reduce_limit config into a number
        if (this.state.query_config && this.state.query_config.reduce_limit &&
            reduce_length > 4096 && ((reduce_length * 2) > input_length)) {
            var log_message = [
                "Reduce output must shrink more rapidly:",
                "input size:", input_length,
                "output size:", reduce_length
            ].join(" ")
            if (this.state.query_config.reduce_limit === "log") {
                this.io.log("reduce_overflow_error: " + log_message)
                this.io.writeline("[true," + reduce_line + "]")
            } else {
                throw(["error", "reduce_overflow_error", log_message])
            }
        } else {
            this.io.print("[true," + reduce_line + "]")
        }
    }

    handleViewError(err, doc) {
        if (err == "fatal_error") {
            // Only if it's a "fatal_error" do we exit. What's a fatal error?
            // That's for the query to decide.
            //
            // This will make it possible for queries to completely error out,
            // by catching their own local exception and rethrowing a
            // fatal_error. But by default if they don't do error handling we
            // just eat the exception and carry on.
            //
            // In this case we abort map processing but don't destroy the
            // JavaScript process. If you need to destroy the JavaScript
            // process, throw the error form matched by the block below.
            throw(["error", "map_runtime_error", "function raised 'fatal_error'"])
        } else if (err[0] == "fatal") {
            // Throwing errors of the form ["fatal","error_key","reason"]
            // will kill the OS process. This is not normally what you want.
            throw(err)
        }
        var message = "function raised exception " +
            (err.toSource ? err.toSource() : err.stack)
        if (doc) message += " with doc._id " + doc._id
        this.io.log(message)
    }

    // view helper function
    emit (key, value) {
        Debug.debug(`emit called`)
        Debug.debug(key)
        Debug.debug(value)
        this.map_results.push([key, value])
    }

    sum (values) {
        var rv = 0
        for (var i in values) {
            rv += values[i]
        }
        return rv
    }

    reduce (reduceFuns, kvs) {
        var keys = new Array(kvs.length)
        var values = new Array(kvs.length)
        for(var i = 0; i < kvs.length; i++) {
            keys[i] = kvs[i][0]
            values[i] = kvs[i][1]
        }
        this.runReduce(reduceFuns, keys, values, false)
    }

    rereduce (reduceFuns, values) {
        this.runReduce(reduceFuns, null, values, true)
    }

    mapDoc (doc) {
        // Compute all the map functions against the document.
        //
        // Each function can output multiple key/value pairs for each document.
        //
        // Example output of map_doc after three functions set by add_fun cmds:
        // [
        //  [["Key","Value"]],                    <- fun 1 returned 1 key value
        //  [],                                   <- fun 2 returned 0 key values
        //  [["Key1","Value1"],["Key2","Value2"]] <- fun 3 returned 2 key values
        // ]
        //

        // Couch.recursivelySeal(doc)

        var buf = []
        for (var fun in this.state.funs) {
            this.map_results = []
            try {
                this.state.funs[fun](doc)
                buf.push(this.map_results)
            } catch (err) {
                Debug.debug(`map_doc err ${err}`)
                this.handleViewError(err, doc)
                // If the error is not fatal, we treat the doc as if it
                // did not emit anything, by buffering an empty array.
                buf.push([])
            }
        }

        this.io.writeline(buf)
    }
}

class Validate {
    constructor (io) {
        this.io = io
    }

    validate (fun, ddoc, args) {
        try {
            fun.apply(ddoc, args);
            this.io.writeline(1);
        } catch (error) {
            if (error.name && error.stack) {
                throw error;
            }
            this.io.writeline(error);
        }
    }
};


class Debug {
    static async debug(message) {
        // await Deno.writeTextFileSync('/tmp/deno-qs.log', message.toString() + '\n')
    }
}

class Couch {
    constructor (qs) {
        this.qs = qs
    }

    compileFunction (source, ddoc, name) {
        if (!source) throw (["error", "not_found", "missing function"]);

        var functionObject = null;
        var sandbox = this.qs.createSandbox();

        try {
            var rewrittenFun = `(${source})` //rewriteFunInt(source);
            const emit = sandbox.emit;
            const sum = sandbox.sum;
            const log = sandbox.log;
            const JSON = sandbox.JSON;
            /* const index = sandbox.index */;
            var newRew = rewrittenFun
            // console.log(newRew)
            Debug.debug('nnnL: ' + newRew)
            functionObject = eval(newRew);
        } catch (err) {
            throw ([
                "error",
                "compilation_error",
                (err.toSource ? err.toSource() : err.stack) + " (" + source + ")"
            ]);
        };
        if (typeof(functionObject) == "function") {
            Debug.debug(`compiled fun ${functionObject}`)

            return functionObject;
        } else {
            throw (["error", "compilation_error",
                "Expression does not eval to a function. (" + source.toString() + ")"
            ]);
        };
    }
}

class QueryServer {
    constructor () {
        this.io = new IO()
        this.couch = new Couch(this)
        this.state = new State(this.couch, this.io)
        this.ddoc = new DesignDoc(this.couch, this.io)
        this.views = new Views(this.couch, this.io, this.state)

        this.dispatch = {
            "ddoc": [this.ddoc, this.ddoc.ddoc],
            "reset": [this.state, this.state.reset],
            "add_fun": [this.state, this.state.addFun],
            "map_doc": [this.views, this.views.mapDoc],
            // "index_doc": Dreyfus.indexDoc,
            "reduce": [this.views, this.views.reduce],
            "rereduce": [this.views, this.views.rereduce]
        }
    }

    createSandbox () {
        // if possible, use evalcx (not always available)
        var sandbox = {}; //eval('');
        sandbox.emit = (k, v) => this.views.emit(k, v);
        sandbox.sum = (k, v, r) => this.views.sum(k, v);
        // sandbox.log = log;
        sandbox.JSON = JSON;
        // sandbox.index = Dreyfus.index;
        return sandbox;
    }

    handleError (e) {
        Debug.debug(e)
        Debug.debug(e.stack)
        var type = e[0]
        if (type == "fatal") {
            e[0] = "error"; // we tell the client it was a fatal error by dying
            this.io.writeline(e)
            Deno.exit(-1)
        } else if (type == "error") {
            this.io.writeline(e)
        } else if (e.error && e.reason) {
            // compatibility with old error format
            this.io.writeline(["error", e.error, e.reason])
        } else if (e.name) {
            this.io.writeline(["error", e.name, e])
        } else {
            this.io.writeline(["error", "unnamed_error", e.toSource ? e.toSource() : e.stack])
        }
    }

    async loop () {
        let cmd
        let cmdkey
        let line
        while (line = await this.io.readline()) {
            cmd = JSON.parse(line)
            Debug.debug(`line: ` + JSON.stringify(cmd) + "\n")
            this.state.line_length = line.length
            try {
                cmdkey = cmd.shift()
                Debug.debug(`cmdkey: ` + JSON.stringify(cmdkey) + "\n")
                Debug.debug(`cmd: ` + JSON.stringify(cmd) + "\n")
                if (this.dispatch[cmdkey]) {
                    // run the correct responder with the cmd body
                    const [object, method] = this.dispatch[cmdkey]
                    method.apply(object, cmd)
                } else {
                    // unknown command, quit and hope the restarted version is better
                    throw (["fatal", "unknown_command", "unknown command '" + cmdkey + "'"])
                }
            } catch (e) {
                this.handleError(e)
            }
        }
    }
}

const query_server = new QueryServer()
query_server.loop()
