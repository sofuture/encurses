%% ============================================================================
%% Encurses 0.4.1
%%
%% Copyright © 2012 Jeff Zellner <jeff.zellner@gmail.com>. All Rights Reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%   1. Redistributions of source code must retain the above copyright notice,
%%      this list of conditions and the following disclaimer.
%%   2. Redistributions in binary form must reproduce the above copyright
%%      notice, this list of conditions and the following disclaimer in the
%%      documentation and/or other materials provided with the distribution.
%%   3. The name of the author may not be used to endorse or promote products
%%      derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED
%% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
%% EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
%% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
%% OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%% ============================================================================

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

