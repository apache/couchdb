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

dev: compile
	@rm -rf rel/dev1 rel/dev2 rel/dev3
	@echo "==> Building development node #1"
	@./rebar generate target_dir=dev1 overlay_vars=dev1.config
	@echo "==> Building development node #2"
	@./rebar generate target_dir=dev2 overlay_vars=dev2.config
	@echo "==> Building development node #3"
	@./rebar generate target_dir=dev3 overlay_vars=dev3.config
