#!/usr/bin/env escript
%%! -noinput -pa ../encurses/ebin +A 10
-include_lib("encurses/include/encurses.hrl").
main(_) -> encurses_demo:go().
