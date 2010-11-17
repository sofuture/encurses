-module(encurses).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-on_load(load_nif/0).

-export([
        addch/1, 
        addstr/1,
        cbreak/0, 
        curs_set/1,
        echo/0,
        endwin/0,
        erase/0,
        erase/1,
        getxy/0,
        getxy/1,
        getmaxxy/0,
        getmaxxy/1,
        has_colors/0,
        initscr/0, 
        move/2,
        nocbreak/0, 
        noecho/0, 
        refresh/0,
        start_color/0
    ]).

%% =============================================================================
%% NIF Loading
%% =============================================================================

load_nif() ->
    erlang:load_nif("./priv/encurses", 0).

%% =============================================================================
%% Application API
%% =============================================================================

refresh() ->
    e_refresh().

endwin() ->
    e_endwin().

initscr() ->
    e_initscr().

cbreak() -> 
    e_cbreak().

nocbreak() ->
    e_nocbreak().

echo() ->
    e_echo().

noecho() ->
    e_noecho().

addch(Char) when is_integer(Char) ->
    e_addch(Char).

erase() ->
    e_erase().

erase(Win) when is_integer(Win) ->
    e_erase(Win).

addstr(String) when is_list(String) ->
    Str = lists:flatten(String),
    e_addstr(erlang:iolist_size(Str), Str).

move(X, Y) when is_integer(X) andalso is_integer(Y) ->
    e_move(X, Y).

getxy() ->
    e_getxy().

getxy(Win) when is_integer(Win) ->
    e_getxy(Win).

getmaxxy() ->
    e_getmaxxy().

getmaxxy(Win) when is_integer(Win) ->
    e_getmaxxy(Win).

curs_set(Flag) when is_integer(Flag) ->
    e_curs_set(Flag).

has_colors() ->
    e_has_colors().

start_color() ->
    e_start_color().

%% =============================================================================
%% Internal functions
%% =============================================================================

e_refresh() ->
    not_initialized.

e_endwin() ->
    not_initialized.

e_initscr() ->
    not_initialized.

e_cbreak() -> 
    not_initialized.

e_nocbreak() -> 
    not_initialized.

e_echo() -> 
    not_initialized.

e_noecho() -> 
    not_initialized.

e_addch(Char) when is_integer(Char) ->
    not_initialized.

e_erase() ->
    not_initialized.

e_erase(Win) when is_integer(Win) ->
    not_initialized.

e_addstr(StrLen, String) when is_integer(StrLen) andalso is_list(String) ->
    not_initialized.

e_move(X, Y) when is_integer(X) andalso is_integer(Y) ->
    not_initialized.

e_getxy() ->
    not_initialized.

e_getxy(Win) when is_integer(Win) ->
    not_initialized.

e_getmaxxy() ->
    not_initialized.

e_getmaxxy(Win) when is_integer(Win) ->
    not_initialized.

e_curs_set(Flag) when is_integer(Flag) ->
    not_initialized.

e_has_colors() ->
    not_initialized.

e_start_color() ->
    not_initialized.
