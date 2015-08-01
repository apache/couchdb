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
check: build test


.PHONY: clean
# target: clean - Prints this help
clean:
	$(REBAR) clean
	rm -f test/*.pyc


.PHONY: distclean
# target: distclean - Removes all unversioned files
distclean: clean
	git clean -fxd


.PHONY: help
# target: help - Prints this help
help:
	@egrep "^# target:" Makefile | sed -e 's/^# target: //g' | sort


.PHONY: test
# target: test - Runs test suite
test:
	nosetests


.PHONY: pip-install
# target: pip-install - Installs requires Python packages
pip-install:
	pip install nose requests


.PHONY: venv
# target: venv - Initializes virtual environment (requires virtualenv)
venv:
	virtualenv --python=python2.7 venv
	@echo "VirtualEnv has been created. Don't forget to run . venv/bin/active"
