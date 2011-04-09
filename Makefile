main: deps compile rel

compile:
	./rebar compile

rel: compile
	(rm -rf rel/json && ./rebar generate)

console: rel
	rel/json/bin/mynode console

deps:
	./rebar get-deps

clean:
	./rebar clean

VERSION=Mailup-0.1
