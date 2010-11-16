-module(encurses_demo).

-compile(export_all).

go() ->
    Win = encurses:initscr(),
    encurses:erase(),
    encurses:refresh(),
    old(Win),
    splash_screen().

old(Win) ->
    encurses:erase(),
    encurses:refresh(),    
    encurses:addch($A),
    encurses:addch($B),
    encurses:addch($C),
    encurses:move(5,5), 
    encurses:addstr("ROFL THIS IS SO GREAT"),
    encurses:move(7,7),
    {CurX, CurY} = encurses:getxy(),
    {CurX, CurY} = encurses:getxy(Win),
    {MaxX, MaxY} = encurses:getmaxxy(),
    {MaxX, MaxY} = encurses:getmaxxy(Win),
    Str = io_lib:format("{~p,~p} - {~p,~p}", [CurX, CurY, MaxX, MaxY]),
    encurses:addstr(Str),
    encurses:refresh(),
    Blink = fun(_) ->
        {X,Y} = encurses:getxy(Win),
        encurses:move(X+1,Y+1),
        encurses:curs_set(1),
        encurses:refresh(),
        timer:sleep(100),
        encurses:curs_set(0),
        encurses:refresh(),
        timer:sleep(100)
    end,
    lists:foreach(Blink, lists:seq(1,20)),
    ok.


spiral(X,Y, DX, DY, MinX, MinY, MaxX, MaxY, Acc) ->
    encurses:move(X, Y),
    encurses:addch($=),
    Acc1 = case Acc of
        5 -> 
            timer:sleep(10),
            encurses:refresh(),
            0;
        _ -> Acc + 1
    end,
    if
        X =:= MaxX andalso DX =:= 1 ->
            spiral(X, Y+1, 0, 1, MinX, MinY, MaxX-1, MaxY, Acc1);
        Y =:= MaxY andalso DY =:= 1 ->
            spiral(X-1, Y, -1, 0, MinX, MinY, MaxX, MaxY-1, Acc1);
        X =:= MinX andalso DX =:= -1 ->
            spiral(X, Y-1, 0, -1, MinX+1, MinY, MaxX, MaxY, Acc1);
        Y =:= MinY andalso DY =:= -1 ->
            spiral(X+1, Y, 1, 0, MinX, MinY+1, MaxX, MaxY, Acc1);
        Y > MaxY+1 orelse X > MaxX+1 orelse X < -2 orelse Y < -2 ->
            ok;
        true ->
            encurses:move(5,5),
            encurses:addstr(io_lib:format("~px~p ~p,~p", [X,Y, MaxX, MaxY])),
            spiral(X+DX, Y+DY, DX, DY, MinX, MinY, MaxX, MaxY, Acc1)
    end.

fade_in_title(Title) ->
    {CX,CY} = centering_coords(length(Title), 1),
    MapChars = fun(Char, Acc) ->
        [{length(Acc), Char} | Acc]
    end,
    Mapped = lists:foldl(MapChars, [], Title),
    Draw = fun({X, Char}) ->
        encurses:move(CX+X, CY -2),
        encurses:addch($\s),
        encurses:move(CX+X, CY -1),
        encurses:addch($\s),
        encurses:move(CX+X, CY),
        encurses:addch($\s),
        encurses:move(CX+X, CY+1),
        encurses:addch($\s),
        encurses:move(CX+X, CY+2),
        encurses:addch($\s),
        encurses:move(CX+X, CY),
        encurses:addch(Char),
        encurses:refresh(),
        timer:sleep(100)
    end,
    random:seed(now()),
    Randomize = fun(_,_) ->
        random:uniform(2) =:= 1
    end,
    lists:foreach(Draw, lists:sort(Randomize, Mapped)).

get_window_dimensions() ->
    {X, Y} = encurses:getmaxxy(),
    {X, Y}.

centering_coords(Width, Height) ->
    {MaxX, MaxY} = get_window_dimensions(),
    X = (MaxX - Width) div 2,
    Y = (MaxY - Height) div 2,
    {X, Y}.

splash_screen() ->
    encurses:erase(),
    {MX,MY} = get_window_dimensions(),
    spiral(0, 0, 1, 0, 0, 0, MX-1, MY-1, 0),
    encurses:refresh(),
    fade_in_title(" -=| E N C U R S E S |=- "),
    timer:sleep(1000),
    encurses:erase(),
    encurses:refresh(),
    encurses:endwin(),
    ok.

