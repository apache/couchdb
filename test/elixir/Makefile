SHELL := /bin/bash

ACTIVATE = source activate

.PHONY: all
all: .elixir/build/bin/mix
	@$(ACTIVATE) ; mix test --trace

.elixir/build/bin/mix:
	@./install_elixir
