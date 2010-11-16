all:
	@./rebar compile

clean:
	@./rebar clean

run: all
	@./run.escript
