all:  compile

compile:
	@ERL_LIBS="`pwd`/apps" ./rebar compile
	python scons/scons.py

clean:
	@./rebar clean

check: compile
	@ERL_LIBS="`pwd`/apps" ./rebar eunit
	@ERL_LIBS="`pwd`/apps" prove apps/couch/test/etap/*.t

compilenative:
	@./rebar compile erl_opts=native

dist: compile
	@rm -rf rel/dbcore
	@./rebar generate
	@mkdir -p rel/dbcore/var/share/server
	@cp build/main.js rel/dbcore/var/share/server/
	@cp build/couchjs rel/dbcore/bin/

distclean: clean
	@rm -rf rel/dbcore
	@rm -f rel/overlay/etc/default.ini
	@rm -f rel/overlay/erts-vsn/bin/erl_call

install: dist
	@mkdir -p {{prefix}}/dbcore/
	@cp -R rel/dbcore/* {{prefix}}/dbcore/
	@mkdir -p {{data}}
	@chown {{user}} {{data}}
	@mkdir -p {{view}}
	@chown {{user}} {{view}}
