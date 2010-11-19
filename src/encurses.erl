-module(encurses).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-on_load(load_nif/0).

-export([
        addch/1, 
        addstr/1,
        attroff/1,
        attroff/2,
        attron/1,
        attron/2,
        border/8,
        border/9,
        box/3,
        cbreak/0, 
        curs_set/1,
        delwin/1,
        echo/0,
        endwin/0,
        erase/0,
        erase/1,
        getch/0,
        getxy/0,
        getxy/1,
        getmaxxy/0,
        getmaxxy/1,
        hline/2,
        hline/3,
        has_colors/0,
        init_pair/3,
        initscr/0, 
        keypad/2,
        move/2,
        move/3,
        mvaddch/3,
        mvaddstr/3,
        mvwaddch/4,
        mvwaddstr/4,
        newwin/4,
        nl/0,
        nocbreak/0,
        noecho/0, 
        nonl/0,
        refresh/0,
        refresh/1,
        scrollok/2,
        sigwinch/0,
        start_color/0,
        vline/2,
        vline/3,
        waddch/2,
        waddstr/2
    ]).

%% =============================================================================
%% NIF Loading
%% =============================================================================

load_nif() ->
    erlang:load_nif("./priv/encurses", 0).

%% =============================================================================
%% Application API
%% =============================================================================

%% refresh

refresh() ->
    e_refresh().

refresh(Win) when is_integer(Win) ->
    e_wrefresh(Win).

%% window management

newwin(Width, Height, StartX, StartY) when is_integer(Width) andalso 
        is_integer(Height) andalso is_integer(StartX) andalso 
        is_integer(StartY) ->
    e_newwin(Width, Height, StartX, StartY).

delwin(Win) when is_integer(Win) ->
    e_delwin(Win).

endwin() ->
    e_endwin().

%% initscr

initscr() ->
    e_initscr().

%% cbreak

cbreak() -> 
    e_cbreak().

nocbreak() ->
    e_nocbreak().

%% echo

echo() ->
    e_echo().

noecho() ->
    e_noecho().

%% erase

erase() ->
    e_erase().

erase(Win) when is_integer(Win) ->
    e_werase(Win).

%% addch

addch(Char) when is_integer(Char) ->
    e_addch(Char).

waddch(Win, Char) when is_integer(Win) andalso is_integer(Char) ->
    e_waddch(Win, Char).

mvaddch(X, Y, Char) when is_integer(X) andalso is_integer(Y) andalso
        is_integer(Char) ->
    e_mvaddch(X, Y, Char).

mvwaddch(Win, X, Y, Char) when is_integer(Win) andalso is_integer(X) andalso
        is_integer(Y) andalso is_integer(Char) ->
    e_mvwaddch(Win, X, Y, Char).

%% addstr

addstr(String) when is_list(String) ->
    Str = lists:flatten(String),
    e_addstr(erlang:iolist_size(Str), Str).

waddstr(Win, String) when is_integer(Win) andalso is_list(String) ->
    Str = lists:flatten(String),
    e_waddstr(Win, erlang:iolist_size(Str), Str).

mvaddstr(X, Y, String) when is_integer(X) andalso is_integer(Y) andalso
        is_list(String) ->
    Str = lists:flatten(String),
    e_mvaddstr(X, Y, erlang:iolist_size(Str), Str).

mvwaddstr(Win, X, Y, String) when is_integer(Win) andalso is_integer(X) andalso
        is_integer(Y) andalso is_list(String) ->
    Str = lists:flatten(String),
    e_mvwaddstr(Win, X, Y, erlang:iolist_size(Str), Str).

%% move

move(X, Y) when is_integer(X) andalso is_integer(Y) ->
    e_move(X, Y).

move(Win, X, Y) when is_integer(Win) andalso is_integer(X) andalso
        is_integer(Y) ->
    e_wmove(Win, X, Y).

%% get x and y

getxy() ->
    e_getxy().

getxy(Win) when is_integer(Win) ->
    e_wgetxy(Win).

getmaxxy() ->
    e_getmaxxy().

getmaxxy(Win) when is_integer(Win) ->
    e_wgetmaxxy(Win).

%% curs_set

curs_set(Flag) when is_integer(Flag) ->
    e_curs_set(Flag).

%% has_colors

has_colors() ->
    e_has_colors().

%% start_color

start_color() ->
    e_start_color().

%% init_pair

init_pair(N, FColor, BColor) when is_integer(N) andalso is_integer(FColor)
        andalso is_integer(BColor) ->
    e_init_pair(N, FColor, BColor).

%% attron/attroff

attron(Mask) when is_integer(Mask) ->
    e_attron(Mask).

attron(Win, Mask) when is_integer(Win) andalso is_integer(Mask) ->
    e_wattron(Win, Mask).

attroff(Mask) when is_integer(Mask) ->
    e_attroff(Mask).

attroff(Win, Mask) when is_integer(Win) andalso is_integer(Mask) ->
    e_wattroff(Win, Mask).

%% nl/nonl

nl() ->
    e_nl().

nonl() ->
    e_nonl().

%% scrollok

scrollok(Win, Flag) when is_integer(Win) andalso is_boolean(Flag) ->
    case Flag of
        true -> e_scrollok(Win, 1);
        false -> e_scrollok(Win, 0)
    end.

%% hline/vline

hline(Char, MaxN) when is_integer(Char) andalso is_integer(MaxN) ->
    e_hline(Char, MaxN).

hline(Win, Char, MaxN) when is_integer(Win) andalso is_integer(Char) 
        andalso is_integer(MaxN) ->
    e_whline(Win, Char, MaxN).

vline(Char, MaxN) when is_integer(Char) andalso is_integer(MaxN) ->
    e_vline(Char, MaxN).

vline(Win, Char, MaxN) when is_integer(Win) andalso is_integer(Char)
        andalso is_integer(MaxN) ->
    e_wvline(Win, Char, MaxN).

%% border

border(Ls, Rs, Ts, Bs, TLs, TRs, BLs, BRs) 
  when is_integer(Ls) andalso is_integer(Rs) andalso 
        is_integer(Ts) andalso is_integer(Bs) andalso is_integer(TLs) andalso
        is_integer(TRs) andalso is_integer(BLs) andalso is_integer(BRs) ->
    e_border(Ls, Rs, Ts, Bs, TLs, TRs, BLs, BRs).

border(Win, Ls, Rs, Ts, Bs, TLs, TRs, BLs, BRs) 
  when is_integer(Win) andalso is_integer(Ls) andalso 
        is_integer(Rs) andalso is_integer(Ts) andalso is_integer(Bs) andalso 
        is_integer(TLs) andalso is_integer(TRs) andalso is_integer(BLs) andalso
        is_integer(BRs) ->
    e_wborder(Win, Ls, Rs, Ts, Bs, TLs, TRs, BLs, BRs).

%% box

box(Win, Horz, Vert) when is_integer(Win) andalso is_integer(Horz) andalso
        is_integer(Vert) ->
    e_box(Win, Horz, Vert).

%% keypad

keypad(Win, Flag) when is_integer(Win) and is_boolean(Flag) ->
    case Flag of
        true -> e_keypad(Win, 1);
        false -> e_keypad(Win, 0)
    end.

%% getch

getch() ->
    e_getch().

%% sigwinch

sigwinch() ->
    e_sigwinch().

%% =============================================================================
%% Internal functions
%% =============================================================================

e_refresh() ->
    not_initialized.

e_wrefresh(_Win) ->
    not_initialized.

e_newwin(_Width, _Height, _StartX, _StartY) ->
    not_initialized.

e_delwin(_Win) ->
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

e_erase() ->
    not_initialized.

e_werase(_Win) ->
    not_initialized.

e_addch(_Char) ->
    not_initialized.

e_waddch(_Win, _Char) ->
    not_initialized.

e_mvaddch(_X, _Y, _Char) ->
    not_initialized.

e_mvwaddch(_Win, _X, _Y, _Char) ->
    not_initialized.

e_addstr(_StrLen, _String) ->
    not_initialized.

e_waddstr(_Win, _StrLen, _String) ->
    not_initialized.

e_mvaddstr(_X, _Y, _StrLen, _String) ->
    not_initialized.

e_mvwaddstr(_Win, _X, _Y, _StrLen, _String) ->
    not_initialized.

e_move(_X, _Y) ->
    not_initialized.

e_wmove(_Win, _X, _Y) ->
    not_initialized.

e_getxy() ->
    not_initialized.

e_wgetxy(_Win) ->
    not_initialized.

e_getmaxxy() ->
    not_initialized.

e_wgetmaxxy(_Win) ->
    not_initialized.

e_curs_set(_Flag) ->
    not_initialized.

e_has_colors() ->
    not_initialized.

e_start_color() ->
    not_initialized.

e_init_pair(_N, _FColor, _BColor) ->
    not_initialized.

e_attron(_Mask) ->
    not_initialized.

e_wattron(_Win, _Mask) ->
    not_initialized.

e_attroff(_Mask) ->
    not_initialized.

e_wattroff(_Win, _Mask) ->
    not_initialized.

e_nl() ->
    not_initialized.

e_nonl() ->
    not_initialized.

e_scrollok(_Win, _Flag) ->
    not_initialized.

e_hline(_Char, _MaxN) ->
    not_initialized.

e_whline(_Win, _Char, _MaxN) ->
    not_initialized.

e_vline(_Char, _MaxN) ->
    not_initialized.

e_wvline(_Win, _Char, _MaxN) ->
    not_initialized.

e_border(_Ls, _Rs, _Ts, _Bs, _TLs, _TRs, _BLs, _BRs) ->
    not_initialized.

e_wborder(_Win, _Ls, _Rs, _Ts, _Bs, _TLs, _TRs, _BLs, _BRs) ->
    not_initialized.

e_box(_Win, _Horz, _Vert) ->
    not_initialized.

e_keypad(_Win, _Flag) ->
    not_initialized.

e_getch() ->
    not_initialized.

e_sigwinch() ->
    not_initialized.
