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

distclean: clean
	@rm -rf rel/dbcore

include install.mk
install: dist
	@mkdir -p $(prefix)
	@cp -R rel/dbcore/* $(prefix)
	@mkdir -p $(data_dir)
	@chown $(user) $(data_dir)
	@mkdir -p $(view_dir)
	@chown $(user) $(view_dir)
	@touch $(prefix)/var/log/dbcore.log
	@chown $(user) $(prefix)/var/log/dbcore.log

dev: compile
	@rm -rf rel/dev1 rel/dev2 rel/dev3
	@echo "==> Building development node #1 (ports 15984/15986)"
	@./rebar generate target_dir=dev1 overlay_vars=dev1.config
	@echo "==> Building development node #2 (ports 25984/25986)"
	@./rebar generate target_dir=dev2 overlay_vars=dev2.config
	@echo "==> Building development node #3 (ports 35984/35986)"
	@./rebar generate target_dir=dev3 overlay_vars=dev3.config
	@echo "\n\
Development nodes are built, and can be started using the dbcore scripts in\n\
./rel/dev[123]/bin.  Once the nodes are started, they must be joined together\n\
by editing the local nodes DB. For example, executing\n\
\n\
    curl localhost:15986/nodes/dev2@127.0.0.1 -X PUT -d '{}'\n\
    curl localhost:15986/nodes/dev3@127.0.0.1 -X PUT -d '{}'\n\
\n\
will cause node 1 to immediately connect to nodes 2 and 3 and form a cluster.\n\
The content of the nodes database is continuously replicated throughout the\n\
cluster, so this is a one-time operation.\n"
