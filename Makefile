.PHONY: all compile test clean

all: compile

exmpp:
	@(cd exmpp-0.9.9; autoreconf -i; \
	 ./configure --prefix=${PWD}/deps --enable-examples; \
	 make install)

compile: exmpp

	./rebar get-deps compile

test: rebar compile
	./rebar skip_deps=true eunit

rmexmpp:
	@(.\rm -rf ${PWD}/deps/exmpp*)
	
clean: rebar rmexmpp
	./rebar clean
	@(cd exmpp-0.9.9; make clean)
		
#rebar:
#	wget http://cloud.github.com/downloads/basho/rebar/rebar
#	chmod u+x rebar
