#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <ncurses.h>
#include "erl_nif.h"

#define BUFF_SIZE 1024

static WINDOW *root;

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
    root = (WINDOW *)initscr();
    return enif_make_long(env, (long)root);
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
    return int_erase(env, root);
}

static ERL_NIF_TERM 
e_werase(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win;
    enif_get_long(env, argv[0], &win);
    return int_erase(env, (WINDOW *)win);
}

// addch

static ERL_NIF_TERM 
e_addch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long ch;
    enif_get_long(env, argv[0], &ch);
    return done(env, addch((chtype)ch));
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

// move

static ERL_NIF_TERM
e_move(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long x, y;
    enif_get_long(env, argv[0], &x);
    enif_get_long(env, argv[1], &y);
    return done(env, move((int)y, (int)x));
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
    return int_getxy(env, root);
}

static ERL_NIF_TERM
e_wgetxy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win;
    enif_get_long(env, argv[0], &win);
    return int_getxy(env, (WINDOW *)win);
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
    return int_getmaxxy(env, root);
}

static ERL_NIF_TERM 
e_wgetmaxxy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win;
    enif_get_long(env, argv[0], &win);
    return int_getmaxxy(env, (WINDOW *)win);
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

static ErlNifFunc nif_funcs[] =
{
    {"e_refresh", 0, e_refresh},
    {"e_endwin", 0, e_endwin},
    {"e_initscr", 0, e_initscr},
    {"e_cbreak", 0, e_cbreak},
    {"e_nocbreak", 0, e_nocbreak},
    {"e_echo", 0, e_echo},
    {"e_noecho", 0, e_noecho},
    {"e_addch", 1, e_addch},
    {"e_addstr", 2, e_addstr},

    {"e_move", 2, e_move},

    {"e_erase", 0, e_erase},
    {"e_erase", 1, e_werase},

    {"e_getxy", 0, e_getxy},
    {"e_getxy", 1, e_wgetxy},

    {"e_getmaxxy", 0, e_getmaxxy},
    {"e_getmaxxy", 1, e_wgetmaxxy},

    {"e_curs_set", 1, e_curs_set},

    {"e_has_colors", 0, e_has_colors},
    {"e_start_color", 0, e_start_color},
};

ERL_NIF_INIT(encurses, nif_funcs, NULL, NULL, NULL, NULL)
