all: compile

deps:
	./rebar get-deps

compile: deps
	./rebar compile

check: compile
	./rebar eunit skip_deps=true

clean:
	./rebar clean

distclean: clean
	rm -fr deps
