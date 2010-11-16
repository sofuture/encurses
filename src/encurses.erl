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

erase() ->
    not_initialized.

erase(Win) when is_integer(Win) ->
    not_initialized.

addstr(String) when is_list(String) ->
    Str = lists:flatten(String),
    e_addstr(erlang:iolist_size(Str), Str).

move(X, Y) when is_integer(X) andalso is_integer(Y) ->
    not_initialized.

getxy() ->
    not_initialized.

getxy(Win) when is_integer(Win) ->
    not_initialized.

getmaxxy() ->
    not_initialized.

getmaxxy(Win) when is_integer(Win) ->
    not_initialized.

curs_set(Flag) when is_integer(Flag) ->
    not_initialized.

has_colors() ->
    not_initialized.

start_color() ->
    not_initialized.

%% =============================================================================
%% Internal functions
%% =============================================================================

e_addstr(Size, String) when is_integer(Size) andalso is_list(String) ->
    not_initialized.
