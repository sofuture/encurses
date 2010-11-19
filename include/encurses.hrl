%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-define(TRUE, 1).
-define(FALSE, 0).

-define(CURS_INVISIBLE, 0).
-define(CURS_NORMAL, 1).
-define(CURS_VERY_VISIBLE, 2).

-define(COLOR_BLACK, 0).
-define(COLOR_RED, 1).
-define(COLOR_GREEN, 2).
-define(COLOR_YELLOW, 3).
-define(COLOR_BLUE, 4).
-define(COLOR_MAGENTA, 5).
-define(COLOR_CYAN, 6).
-define(COLOR_WHITE, 7).

-define(A_NORMAL, 0).
-define(COLOR_PAIR(C), (C bsl 8)).
-define(A_BOLD, (1 bsl (8 + 13))).
-define(A_UNDERLINE, (1 bsl (8 + 9))).
-define(A_REVERSE, (1 bsl (8 + 10))).
-define(A_BLINK, (1 bsl (8 + 11))).

-define(STDSCR, 0).

-define(ACS_DIAMOND, 4194400).
-define(ACS_CKBOARD, 4194401).
-define(ACS_DEGREE, 4194406).
-define(ACS_PLMINUS, 4194407).
-define(ACS_BOARD, 4194408).
-define(ACS_LANTERN, 4194409).
-define(ACS_LRCORNER, 4194410).
-define(ACS_URCORNER, 4194411).
-define(ACS_ULCORNER, 4194412).
-define(ACS_LLCORNER, 4194413).
-define(ACS_PLUS, 4194414).
-define(ACS_S1, 4194415).
-define(ACS_S3, 4194416).
-define(ACS_HLINE, 4194417).
-define(ACS_S7, 4194418).
-define(ACS_S9, 4194419).
-define(ACS_LTEE, 4194420).
-define(ACS_RTEE, 4194421).
-define(ACS_BTEE, 4194422).
-define(ACS_TTEE, 4194423).
-define(ACS_VLINE, 4194424).
-define(ACS_LEQUAL, 4194425).
-define(ACS_GEQUAL, 4194426).
-define(ACS_PI, 4194427).
-define(ACS_NEQUAL, 4194428).
-define(ACS_STERLING, 4194429).
-define(ACS_BULLET, 4194430).
-define(ACS_RARROW, 4194347).
-define(ACS_LARROW, 4194348).
-define(ACS_UARROW, 4194349).
-define(ACS_DARROW, 4194350).
-define(ACS_BLOCK, 4194352).

-define(KEY_TAB, 9).
-define(KEY_ESC, 27).


-define(KEY_DOWN, 258).
-define(KEY_UP, 259).
-define(KEY_LEFT, 260).
-define(KEY_RIGHT, 261).
-define(KEY_HOME, 262).
-define(KEY_F(N), 264+N).
-define(KEY_DEL, 330).
-define(KEY_INS, 331).
-define(KEY_PGDOWN, 338).
-define(KEY_PGUP, 339).
-define(KEY_END, 360).


