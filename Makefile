all:
	@./rebar compile

clean:
	@./rebar clean
	$(if $(wildcard ebin/), rmdir ebin/)
	$(if $(wildcard priv/), rmdir priv/)

run: all
	@./run.escript
