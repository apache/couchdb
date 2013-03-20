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


etap: test/etap.beam test/tutil.beam
	prove test/*.t


check: build etap


%.beam: %.erl
	erlc -o test/ $<


.PHONY: all clean distclean deps build eunit check
