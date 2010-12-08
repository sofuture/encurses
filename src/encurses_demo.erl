%% ============================================================================
%% Encurses 0.4.0
%%
%% Copyright 2010 Jeff Zellner
%%
%% This software is provided with absolutely no assurances, guarantees, 
%% promises or assertions whatsoever.
%%
%% Do what thou wilt shall be the whole of the law.
%% ===========================================================================

-module(encurses_demo).
-include("encurses.hrl").
-compile(export_all).

go() ->
    encurses:initscr(),
    encurses:keypad(0, true),
    encurses:curs_set(?CURS_INVISIBLE),
    encurses:erase(),
    encurses:refresh(),

    {Mx,My} = encurses:getmaxxy(),
    Str = "Hello World! - encurses!",
    StrLen = length(Str),
    Timer = erlang:start_timer(10000, main, times_up),

    spawn(?MODULE, keyloop, []),
    spawn(?MODULE, other_win, []),
    spawn(?MODULE, bounce_text, [Str, StrLen, 0, 0, 1, 1]),
    spawn(?MODULE, bounce_timer, [Timer, Mx div 2, My div 2, -1, 1]),
    register(main, self()),
    receive 
        {timeout, Timer, times_up} ->
            encurses:erase(),
            encurses:refresh(),
            encurses:endwin()
    end.

bounce_timer(Timer, PrevX, PrevY, DirX, DirY) ->
    Str = io_lib:format("~p", [erlang:read_timer(Timer)]),
    encurses:move(PrevX, PrevY),
    encurses:hline($\s, 4),
    {NewX, NewY, NewDirX, NewDirY} = calc_new_pos(4, PrevX, PrevY, DirX, DirY),
    encurses:mvaddstr(NewX, NewY, Str),
    encurses:refresh(),
    timer:sleep(20),
    bounce_timer(Timer, NewX, NewY, NewDirX, NewDirY).

bounce_text(Str, StrLen, PrevX, PrevY, DirX, DirY) ->
    encurses:move(PrevX, PrevY),
    encurses:hline($\s, length(Str)),
    {NewX, NewY, NewDirX, NewDirY} = calc_new_pos(StrLen, PrevX, PrevY, DirX, DirY),
    encurses:mvaddstr(NewX, NewY, Str),
    encurses:refresh(),
    timer:sleep(100),
    bounce_text(Str, StrLen, NewX, NewY, NewDirX, NewDirY).

calc_new_pos(Len, Px, Py, Dx, Dy) ->
    {Mx, My} = encurses:getmaxxy(),
    {NewPy, NewDy} =
    if 
        (Py+(Dy) >= My) orelse (Py+(Dy) < 0) ->
            {Py+(Dy*-1), Dy*-1};
        true ->
            {Py+(Dy), Dy}
    end,
    {NewPx, NewDx} =
    if 
        (Px+(Dx)+Len >= Mx) orelse (Px+(Dx) < 0) ->
            {Px+(Dx*-1), Dx*-1};
        true ->
            {Px+(Dx), Dx}
    end,
    {NewPx, NewPy, NewDx, NewDy}.


other_win() ->
    Win = encurses:newwin(5,5,5,5),
    encurses:border(Win, $|, $|, $-, $-, $+, $+, $+, $+),
    encurses:mvwaddstr(Win, 1, 1, "sup"),
    encurses:mvwaddstr(Win, 1, 2, "sup"),
    encurses:mvwaddstr(Win, 1, 3, "sup"),
    encurses:refresh(Win),
    timer:sleep(100),
    other_win(Win).

other_win(Win) ->
    encurses:delwin(Win),
    other_win().

refresh(Win) ->
    encurses:refresh(Win),
    timer:sleep(100),
    refresh(Win).

keyloop() ->
    encurses:noecho(),
    Ch = encurses:getch(),
    encurses:mvaddstr(10,10,"        "),
    encurses:mvaddstr(10,10,io_lib:format("~p",[parse_direction(Ch)])),
    keyloop().

parse_direction(Char) ->
    case Char of
        262 -> kp_nw;
        $7 -> kp_nw;
        259 -> kp_n;
        $8 -> kp_n;
        339 -> kp_ne;
        $9 -> kp_ne;
        260 -> kp_w;
        $4 -> kp_w;
        350 -> kp_center;
        $5 -> kp_center;
        261 -> kp_e;
        $6 -> kp_e;
        360 -> kp_sw;
        $1 -> kp_sw;
        258 -> kp_s;
        $2 -> kp_s;
        338 -> kp_se;
        $3 -> kp_se;
        Other -> Other
    end.

