1. Getting Started

Cloudant-core uses a rebar-based build system.  Custom build paths and the like
are available using templates.  If you want to customize those templates, run
the `bootstrap` command to install the defaults into ~/.rebar and go from there.
This step is optional.

2. Dependencies

 * Erlang R13B03 or higher
 * ICU (4.2 is preferable)
 * Spidermonkey (1.9.2 preferable, [https://launchpad.net/~commonjs/+archive/ppa/][6]) and symlink /usr/lib/libmozjs-1.9.2.so to /usr/lib/libmozjs.so to make things easier
 * OpenSSL
 * make

On OS X the Erlang and ICU dependencies are easily satisfied by Homebrew:

    brew install erlang icu4c
    brew ln icu4c

Once the dependencies are installed you can use `make` to build the code,
`make check` to run unit tests, and `make clean` to reset.

3. Release Generation

The `make dist` command will build a standard release in `rel/dbcore`.  The
release includes a start script at `rel/dbcore/bin/dbcore`

4. Caveats

The build system really really wants the couch .ini files to be in
`/opt/dbcore/etc`, and the data files to be in `/srv`.  It wouldn't be too
difficult to template this.  But for now, the recommended installation is to move the rel/dbcore folder to /opt/
