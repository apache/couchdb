.PHONY: rel deps test

all: deps compile

compile:
	rebar compile

deps:
	rebar get-deps

clean:
	rebar clean

distclean: clean
	rebar delete-deps

test:
	rebar compile eunit

escriptize:
	rebar escriptize

##
## Doc targets
##
docs:
	rebar doc skip_deps=true
