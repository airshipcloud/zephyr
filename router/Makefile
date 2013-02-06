all: deps compile generate

compile:
	./rebar compile
	mkdir -p priv
	cp ../config/routes.json priv

deps:
	./rebar get-deps

clean:
	./rebar clean
	rm -fr priv

generate:
	./rebar generate
