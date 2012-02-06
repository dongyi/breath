.PHONY: all clean test test-fast docs build_plt analyze update \
    erlang test_erlang test_erlang_fast clean_erlang docs_erlang \

REBAR?=./rebar

all: erlang

clean: clean_erlang

test: test_erlang

test-fast: test_erlang_fast

update: update_erlang

docs: docs_erlang

erlang:
	@$(REBAR) get-deps
	@$(REBAR) compile

update_erlang:
	@$(REBAR) update-deps

compile_erlang:
	@$(REBAR) compile

test_erlang: erlang
	@rm -rf .eunit # Do not like: #14639
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

test_erlang_fast: compile_erlang
	@$(REBAR) skip_deps=true eunit

clean_erlang:
	@$(REBAR) delete-deps
	@$(REBAR) clean
	@rm -rf .eunit
	@rm -rf doc

docs_erlang:
	@$(REBAR) skip_deps=true doc