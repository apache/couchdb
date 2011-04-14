.PHONY: rel stagedevrel deps test

all: deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean

distclean: clean
	./rebar delete-deps

test:
	./rebar compile eunit

escriptize:
	./rebar escriptize

##
## Doc targets
##
docs:
	./rebar doc skip_deps=true

pages: docs
	cp priv/index.html doc/_index.html
	cp priv/ForkMe_Blk.png doc/
	cp priv/*.jpg doc/
	git checkout gh-pages
	mv doc/_index.html ./index.html
	mv doc/ForkMe_Blk.png .
	mv doc/*.jpg .
	rm -rf edoc/*
	cp -R doc/* edoc/
	git add .
	git add -u
	git commit
	git push origin gh-pages
	git checkout master

##
## Release targets
##
VSN = `grep vsn src/riaknostic.app.src | cut -f 2 -d "\""`

package: all docs
	@mkdir -p pkg/riaknostic
	@rm -rf pkg/riaknostic/*
	@cat .manifest | xargs -n 1 -I % cp -R % pkg/riaknostic/.
	@tar -czf pkg/riaknostic-$(VSN).tar.gz -C pkg riaknostic

##
## Dialyzer targets
##
APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = $(HOME)/.riak_combo_dialyzer_plt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS)

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS)

dialyzer: compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) ebin deps/*/ebin | \
	    fgrep -v -f ./dialyzer.ignore-warnings

cleanplt:
	@echo 
	@echo "Are you sure?  It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo 
	sleep 5
	rm $(COMBO_PLT)

