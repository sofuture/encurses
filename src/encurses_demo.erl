-module(encurses_demo).

-compile(export_all).

go() ->
    Win = encurses:initscr(),
    encurses:erase(),
    encurses:refresh(),
    Str = "Hello World! - encurses!",
    StrLen = length(Str),
    bounce_text(Str, StrLen, 0, 0, 1, 1),
    ok.

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


