all:  compile

compile:
	@echo "==> couchjs (compile)"
	@cd couchjs && python scons/scons.py
	@./rebar compile

clean:
	@echo "==> couchjs (clean)"
	@cd couchjs && python scons/scons.py --clean
	@./rebar clean

check:
	@./rebar eunit
	@ERL_LIBS="`pwd`/apps" prove apps/couch/test/etap/*.t

dist: compile
	@rm -rf rel/dbcore
	@./rebar generate
	@mkdir -p rel/dbcore/var/share/server
	@cp couchjs/build/main.js rel/dbcore/var/share/server/
	@cp couchjs/build/couchjs rel/dbcore/bin/

distclean: clean
	@rm -rf rel/dbcore
	@rm -f rel/overlay/etc/default.ini

install: dist
	@mkdir -p {{prefix}}/dbcore/
	@cp -R rel/dbcore/* {{prefix}}/dbcore/
	@mkdir -p {{data_dir}}
	@chown {{user}} {{data_dir}}
	@mkdir -p {{view_dir}}
	@chown {{user}} {{view_dir}}
