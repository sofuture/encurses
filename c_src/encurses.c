#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <ncurses.h>
#include "erl_nif.h"

#define _MAXWINDOWS 64

static WINDOW *slots[_MAXWINDOWS+1];

/** function prototypes **/

/* NIF management */

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_data);

/* internal helpers */

static int find_free_window_slot();
static ERL_NIF_TERM done(ErlNifEnv* env, int code);
static ERL_NIF_TERM boolean(ErlNifEnv* env, int value);

/* curses helpers */

static ERL_NIF_TERM int_erase(ErlNifEnv* env, WINDOW *win);
static ERL_NIF_TERM int_getxy(ErlNifEnv* env, WINDOW *win);
static ERL_NIF_TERM int_getmaxxy(ErlNifEnv* env, WINDOW *win);

/* more or less curses interface */

static ERL_NIF_TERM e_refresh(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_wrefresh(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM e_newwin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_delwin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_endwin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM e_initscr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM e_cbreak(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_nocbreak(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM e_echo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_noecho(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM e_erase(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_werase(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM e_addch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_waddch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_mvaddch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_mvwaddch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM e_addstr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_waddstr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_mvaddstr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_mvwaddstr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM e_move(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_wmove(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM e_getxy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_wgetxy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_getmaxxy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_wgetmaxxy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM e_curs_set(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM e_has_colors(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_start_color(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM e_init_pair(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM e_attron(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_wattron(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_attroff(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_wattroff(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM e_nl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_nonl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM e_scrollok(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM e_hline(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_whline(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_vline(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_wvline(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM e_border(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM e_wborder(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM e_box(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM e_keypad(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM e_getch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

/** implementations **/

/* NIF management */

static int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    int i;
    for(i=0;i<_MAXWINDOWS;i++){
        slots[i] = NULL;
    }
    return 0;
}

/* internal helper functions */

static int
find_free_window_slot()
{
    int i;
    for(i=1;i<_MAXWINDOWS;i++){
        if(slots[i] == NULL)
            return i;
    }
    return -1;
}

static ERL_NIF_TERM 
done(ErlNifEnv* env, int code)
{
    if(code == OK)
    {
        return enif_make_atom(env, "ok");
    }
    else 
    {
        return enif_make_tuple2(env,
                enif_make_atom(env, "error"),
                enif_make_int(env, code));
    }
}

static ERL_NIF_TERM
boolean(ErlNifEnv* env, int value) 
{
    if(value == TRUE)
    {
        return enif_make_atom(env, "true");
    }
    else 
    {
        return enif_make_atom(env, "false");
    }
}

// refresh

static ERL_NIF_TERM 
e_refresh(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return done(env, refresh());
}

static ERL_NIF_TERM 
e_wrefresh(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win;
    enif_get_long(env, argv[0], &win);
    return done(env, wrefresh(slots[win]));
}

// newwin

static ERL_NIF_TERM 
e_newwin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int slot = find_free_window_slot();
    long width, height, startx, starty;
    enif_get_long(env, argv[0], &width);
    enif_get_long(env, argv[1], &height);
    enif_get_long(env, argv[2], &startx);
    enif_get_long(env, argv[3], &starty);
    slots[slot] = newwin(height, width, starty, startx);
    return enif_make_long(env, slot);
}

// delwin

static ERL_NIF_TERM 
e_delwin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long slot;
    enif_get_long(env, argv[0], &slot);
    if(slot == 0) 
    {
        return done(env, FALSE);
    }
    else if(slots[slot] == NULL) 
    {
        return done(env, FALSE);
    }
    else 
    {
        delwin(slots[slot]);
        return done(env, OK);
    }
}

// endwin

static ERL_NIF_TERM 
e_endwin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return done(env, endwin());
}

// initscr

static ERL_NIF_TERM 
e_initscr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    slots[0] = (WINDOW *)initscr();
    return enif_make_long(env, 0);
}

// cbreak

static ERL_NIF_TERM 
e_cbreak(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return done(env, cbreak());
}

// nocbreak

static ERL_NIF_TERM 
e_nocbreak(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return done(env, nocbreak());
}

// echo

static ERL_NIF_TERM 
e_echo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return done(env, echo());
}

// noecho

static ERL_NIF_TERM 
e_noecho(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return done(env, noecho());
}

// erase

static ERL_NIF_TERM 
int_erase(ErlNifEnv* env, WINDOW *win)
{
    return done(env, werase(win));
}

static ERL_NIF_TERM 
e_erase(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return int_erase(env, slots[0]);
}

static ERL_NIF_TERM 
e_werase(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win;
    enif_get_long(env, argv[0], &win);
    return int_erase(env, slots[win]);
}

// addch

static ERL_NIF_TERM 
e_addch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long ch;
    enif_get_long(env, argv[0], &ch);
    return done(env, addch((chtype)ch));
}

static ERL_NIF_TERM 
e_waddch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win, ch;
    enif_get_long(env, argv[0], &win);
    enif_get_long(env, argv[1], &ch);
    return done(env, waddch(slots[win], (chtype)ch));
}

static ERL_NIF_TERM 
e_mvaddch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long x, y, ch;
    enif_get_long(env, argv[0], &x);
    enif_get_long(env, argv[1], &y);
    enif_get_long(env, argv[2], &ch);
    return done(env, mvaddch((int)y, (int)x, (chtype)ch));
}

static ERL_NIF_TERM 
e_mvwaddch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win, x, y, ch;
    enif_get_long(env, argv[0], &win);
    enif_get_long(env, argv[1], &x);
    enif_get_long(env, argv[2], &y);
    enif_get_long(env, argv[3], &ch);
    return done(env, mvwaddch(slots[win], (int)y, (int)x, (chtype)ch));
}

// addstr

static ERL_NIF_TERM
e_addstr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long length;
    enif_get_long(env, argv[0], &length);
    char buff[length];
    enif_get_string(env, argv[1], buff, length+1, ERL_NIF_LATIN1);

    return done(env, addnstr(buff, length));
}

static ERL_NIF_TERM 
e_waddstr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long length;
    enif_get_long(env, argv[1], &length);
    char buff[length];
    enif_get_string(env, argv[2], buff, length+1, ERL_NIF_LATIN1);

    long win;
    enif_get_long(env, argv[0], &win);

    return done(env, waddnstr(slots[win], buff, length));
}

static ERL_NIF_TERM 
e_mvaddstr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long length;
    enif_get_long(env, argv[2], &length);
    char buff[length];
    enif_get_string(env, argv[3], buff, length+1, ERL_NIF_LATIN1);

    long x, y;
    enif_get_long(env, argv[0], &x);
    enif_get_long(env, argv[1], &y);
    return done(env, mvaddnstr((int)y, (int)x, buff, length));
}

static ERL_NIF_TERM 
e_mvwaddstr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long length;
    enif_get_long(env, argv[0], &length);
    char buff[length];
    enif_get_string(env, argv[1], buff, length+1, ERL_NIF_LATIN1);

    long win, x, y;
    enif_get_long(env, argv[0], &win);
    enif_get_long(env, argv[1], &x);
    enif_get_long(env, argv[2], &y);
    return done(env, mvwaddnstr(slots[win], (int)y, (int)x, buff, length));
}

// move

static ERL_NIF_TERM
e_move(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long x, y;
    enif_get_long(env, argv[0], &x);
    enif_get_long(env, argv[1], &y);
    return done(env, move((int)y, (int)x));
}

static ERL_NIF_TERM
e_wmove(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win, x, y;
    enif_get_long(env, argv[0], &win);
    enif_get_long(env, argv[1], &x);
    enif_get_long(env, argv[2], &y);
    return done(env, wmove(slots[win], (int)y, (int)x));
}

// getxy

static ERL_NIF_TERM
int_getxy(ErlNifEnv* env, WINDOW *win)
{
    long x, y;
    getyx(win, y, x);
    return enif_make_tuple2(env,
        enif_make_int(env, (int) x),
        enif_make_int(env, (int) y));
}

static ERL_NIF_TERM
e_getxy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return int_getxy(env, slots[0]);
}

static ERL_NIF_TERM
e_wgetxy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win;
    enif_get_long(env, argv[0], &win);
    return int_getxy(env, slots[win]);
}

// getmaxxy

static ERL_NIF_TERM
int_getmaxxy(ErlNifEnv* env, WINDOW *win)
{
    long x, y;
    getmaxyx(win, y, x);
    return enif_make_tuple2(env,
        enif_make_int(env, (int) x),
        enif_make_int(env, (int) y));
}

static ERL_NIF_TERM
e_getmaxxy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return int_getmaxxy(env, slots[0]);
}

static ERL_NIF_TERM 
e_wgetmaxxy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win;
    enif_get_long(env, argv[0], &win);
    return int_getmaxxy(env, slots[win]);
}

// curs_set

static ERL_NIF_TERM 
e_curs_set(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long flag;
    enif_get_long(env, argv[0], &flag);
    return done(env, curs_set((int)flag));
}

// has_colors

static ERL_NIF_TERM 
e_has_colors(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return boolean(env, has_colors());
}

// start_colors

static ERL_NIF_TERM 
e_start_color(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return done(env, start_color());
}

// init_pair

static ERL_NIF_TERM
e_init_pair(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long n, fcolor, bcolor;
    enif_get_long(env, argv[0], &n);
    enif_get_long(env, argv[1], &fcolor);
    enif_get_long(env, argv[2], &bcolor);
    return done(env, init_pair((int)n, (int)fcolor, (int)bcolor));
}

// attron/attroff

static ERL_NIF_TERM 
e_attron(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long attrs;
    enif_get_long(env, argv[0], &attrs);
    return done(env, wattron(slots[0], (int)attrs));
}

static ERL_NIF_TERM 
e_wattron(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win, attrs;
    enif_get_long(env, argv[0], &win);
    enif_get_long(env, argv[1], &attrs);
    return done(env, wattron(slots[win], (int)attrs));
}

static ERL_NIF_TERM 
e_attroff(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long attrs;
    enif_get_long(env, argv[0], &attrs);
    return done(env, wattroff(slots[0], (int)attrs));
}

static ERL_NIF_TERM 
e_wattroff(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win, attrs;
    enif_get_long(env, argv[0], &win);
    enif_get_long(env, argv[1], &attrs);
    return done(env, wattroff(slots[win], (int)attrs));
}

// nl/nonl

static ERL_NIF_TERM 
e_nl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return done(env, nl());
}

static ERL_NIF_TERM 
e_nonl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return done(env, nonl());
}

// scrollok

static ERL_NIF_TERM 
e_scrollok(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win, flag;
    enif_get_long(env, argv[0], &win);
    enif_get_long(env, argv[1], &flag);
    return done(env, scrollok(slots[win], (int)flag));
}

// vline/hline

static ERL_NIF_TERM 
e_hline(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long ch, max;
    enif_get_long(env, argv[0], &ch);
    enif_get_long(env, argv[1], &max);
    return done(env, whline(slots[0], (chtype)ch, (int)max));
}

static ERL_NIF_TERM 
e_whline(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win, ch, max;
    enif_get_long(env, argv[0], &win);
    enif_get_long(env, argv[1], &ch);
    enif_get_long(env, argv[2], &max);
    return done(env, whline(slots[win], (chtype)ch, (int)max));
}

static ERL_NIF_TERM 
e_vline(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long ch, max;
    enif_get_long(env, argv[0], &ch);
    enif_get_long(env, argv[1], &max);
    return done(env, wvline(slots[0], (chtype)ch, (int)max));
}

static ERL_NIF_TERM 
e_wvline(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win, ch, max;
    enif_get_long(env, argv[0], &win);
    enif_get_long(env, argv[1], &ch);
    enif_get_long(env, argv[2], &max);
    return done(env, wvline(slots[win], (chtype)ch, (int)max));
}

// border

static ERL_NIF_TERM 
e_border(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long ls, rs, ts, bs, tl, tr, bl, br;
    enif_get_long(env, argv[0], &ls);
    enif_get_long(env, argv[1], &rs);
    enif_get_long(env, argv[2], &ts);
    enif_get_long(env, argv[3], &bs);
    enif_get_long(env, argv[4], &tl);
    enif_get_long(env, argv[5], &tr);
    enif_get_long(env, argv[6], &bl);
    enif_get_long(env, argv[7], &br);
    return done(env, wborder(slots[0], (chtype)ls, (chtype)rs, (chtype)ts,
            (chtype)bs, (chtype)tl, (chtype)tr, (chtype)bl, (chtype)br));
}

static ERL_NIF_TERM 
e_wborder(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win, ls, rs, ts, bs, tl, tr, bl, br;
    enif_get_long(env, argv[0], &win);
    enif_get_long(env, argv[1], &ls);
    enif_get_long(env, argv[2], &rs);
    enif_get_long(env, argv[3], &ts);
    enif_get_long(env, argv[4], &bs);
    enif_get_long(env, argv[5], &tl);
    enif_get_long(env, argv[6], &tr);
    enif_get_long(env, argv[7], &bl);
    enif_get_long(env, argv[8], &br);
    return done(env, wborder(slots[win], (chtype)ls, (chtype)rs, (chtype)ts,
            (chtype)bs, (chtype)tl, (chtype)tr, (chtype)bl, (chtype)br));
}

// box

static ERL_NIF_TERM 
e_box(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win, hch, vch;
    enif_get_long(env, argv[0], &win);
    enif_get_long(env, argv[1], &hch);
    enif_get_long(env, argv[2], &vch);
    return done(env, box(slots[win], (chtype)vch, (chtype)hch));
}

// keypad

static ERL_NIF_TERM 
e_keypad(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win, flag;
    enif_get_long(env, argv[0], &win);
    enif_get_long(env, argv[1], &flag);
    return done(env, keypad(slots[win], (int)flag));
}

// getch

static ERL_NIF_TERM 
e_getch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return done(env, OK);
}

/* NIF map */

static ErlNifFunc nif_funcs[] =
{
    {"e_refresh", 0, e_refresh},
    {"e_wrefresh", 1, e_wrefresh},

    {"e_newwin", 4, e_newwin},
    {"e_delwin", 1, e_delwin},
    {"e_endwin", 0, e_endwin},

    {"e_initscr", 0, e_initscr},

    {"e_cbreak", 0, e_cbreak},
    {"e_nocbreak", 0, e_nocbreak},

    {"e_echo", 0, e_echo},
    {"e_noecho", 0, e_noecho},

    {"e_erase", 0, e_erase},
    {"e_werase", 1, e_werase},

    {"e_addch", 1, e_addch},
    {"e_waddch", 2, e_waddch},
    {"e_mvaddch", 3, e_mvaddch},
    {"e_mvwaddch", 4, e_mvwaddch},

    {"e_addstr", 2, e_addstr},
    {"e_waddstr", 3, e_waddstr},
    {"e_mvaddstr", 4, e_mvaddstr},
    {"e_mvwaddstr", 5, e_mvwaddstr},

    {"e_move", 2, e_move},
    {"e_wmove", 3, e_wmove},

    {"e_getxy", 0, e_getxy},
    {"e_wgetxy", 1, e_wgetxy},
    {"e_getmaxxy", 0, e_getmaxxy},
    {"e_wgetmaxxy", 1, e_wgetmaxxy},

    {"e_curs_set", 1, e_curs_set},

    {"e_has_colors", 0, e_has_colors},
    {"e_start_color", 0, e_start_color},

    {"e_init_pair", 3, e_init_pair},

    {"e_attron", 1, e_attron},
    {"e_wattron", 2, e_wattron},
    {"e_attroff", 1, e_attroff},
    {"e_wattroff", 2, e_wattroff},

    {"e_nl", 0, e_nl},
    {"e_nonl", 0, e_nonl},

    {"e_scrollok", 2, e_scrollok},

    {"e_hline", 2, e_hline},
    {"e_whline", 3, e_whline},
    {"e_vline", 2, e_vline},
    {"e_wvline", 3, e_wvline},

    {"e_border", 8, e_border},
    {"e_wborder", 9, e_wborder},

    {"e_box", 3, e_box},

    {"e_keypad", 2, e_keypad},
    {"e_getch", 0, e_getch},
};

ERL_NIF_INIT(encurses, nif_funcs, load, NULL, NULL, NULL)
