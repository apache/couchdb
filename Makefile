REBAR?=./rebar


all: build


clean:
	$(REBAR) clean
	rm -rf logs
	rm -rf .eunit
	rm -f test/*.beam


distclean: clean
	git clean -fxd


deps:
	@if test ! -d ./deps; then \
		$(REBAR) get-deps; \
	fi


build: deps
	$(REBAR) compile


eunit:
	$(REBAR) eunit skip_deps=true


check: build eunit


.PHONY: all clean distclean deps build eunit check
