-module(encurses_demo).

-compile(export_all).

go() ->
    ok = application:start(encurses),
    ok = encurses:start(ok,ok),
    Win = encurses:initscr(),
    encurses:werase(Win),
    encurses:refresh(),    
    encurses:addch($A),
    encurses:addch($B),
    encurses:addch($C),
    encurses:addch($D),
    encurses:addch($E),
    encurses:addch($F),
    encurses:addch($G),
    encurses:addch($H),
    encurses:addch($I),
    encurses:addch($J),
    encurses:move(5,5), 
    encurses:addstr("ROFL THIS IS SO GREAT"),
    encurses:refresh(),
    timer:sleep(3000),
    encurses:endwin().
