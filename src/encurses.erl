-module(encurses).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-behaviour(application).

-export([start/2, stop/1]).

-export([
        addch/1, 
        cbreak/0, 
        echo/0,
        endwin/0, 
        initscr/0, 
        nocbreak/0, 
        noecho/0, 
        refresh/0,
        werase/1
    ]).

%% =============================================================================
%% Application API
%% =============================================================================

start(_,_) ->
    erlang:load_nif("../priv/encurses", 0),
    ok.

stop(_) ->
    ok.

%% =============================================================================
%% Application API
%% =============================================================================

refresh() ->
    not_initialized.

endwin() ->
    not_initialized.

initscr() ->
    not_initialized.

cbreak() -> 
    not_initialized.

nocbreak() -> 
    not_initialized.

echo() -> 
    not_initialized.

noecho() -> 
    not_initialized.

addch(Char) when is_integer(Char) ->
    not_initialized.

werase(Win) when is_integer(Win) ->
    not_initialized.

