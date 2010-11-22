-module(encurses_demo).
-include("encurses.hrl").
-compile(export_all).

go() ->
    encurses:initscr(),
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
    encurses:mvaddch(5,1 , Ch),
    encurses:mvaddch(6,1, Ch),
    encurses:mvaddch(7,1, Ch),
    encurses:mvaddch(8,1, Ch),
    keyloop().
