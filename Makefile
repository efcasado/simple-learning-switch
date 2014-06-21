.PHONY: deps rel

all: rel

rel: compile
	cd rel; ../rebar generate

compile: deps
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean
	@rm -rf ./rel/lswitch

start:
	@./rel/lswitch/bin/lswitch start

stop:
	@./rel/lswitch/bin/lswitch stop

attach:
	@./rel/lswitch/bin/lswitch attach
