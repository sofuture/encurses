#!/usr/bin/env escript
%%! -noinput -pa ./ebin +A 10
-include_lib("include/encurses.hrl").
main(_) -> encurses_demo:go().
