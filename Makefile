all: compile dialyzer

compile:
	@./rebar3 compile

dialyzer:
	@./rebar3 dialyzer

clean:
	@./rebar3 clean
	$(if $(wildcard _build/), rm -rf _build/)
	$(if $(wildcard ebin/), rm -rf ebin/)
	$(if $(wildcard priv/), rm -rf priv/)

run: compile
	@./run.escript
