all: build release

build:
	rebar3 compile

release:
	rebar3 release

run: build release
	./_build/default/rel/mixtape_test/bin/mixtape_test console
