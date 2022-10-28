REBAR?=rebar


.PHONY: all
# target: all - Makes everything
all: build


.PHONY: build
# target: build - Builds the project
build:
	$(REBAR) compile


.PHONY: check
# target: check - Checks if project builds and passes all the tests
check: build eunit


.PHONY: clean
# target: clean - Removes build artifacts
clean:
	$(REBAR) clean


.PHONY: distclean
# target: distclean - Removes all unversioned files
distclean: clean
	git clean -fxd


.PHONY: eunit
# target: eunit - Runs eunit test suite
eunit:
	$(REBAR) eunit


.PHONY: help
# target: help - Prints this help
help:
	@egrep "^# target:" Makefile | sed -e 's/^# target: //g' | sort
