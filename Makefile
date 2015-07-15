SHELL=/bin/bash

deps:
	@./rebar get-deps
	@./rebar update-deps

compile:
	@./rebar compile

console: deps compile
	@erl -pa apps/*/ebin deps/*/ebin -s erebus -s erebus_http
