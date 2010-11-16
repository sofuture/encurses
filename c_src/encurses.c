#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <ncurses.h>
#include "erl_nif.h"

#define BUFF_SIZE 1024

static ERL_NIF_TERM done(ErlNifEnv* env, int code)
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

static ERL_NIF_TERM e_refresh(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return done(env, refresh());
}

static ERL_NIF_TERM e_endwin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return done(env, endwin());
}

static ERL_NIF_TERM e_initscr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    WINDOW *win = (WINDOW *)initscr();
    return enif_make_long(env, (long)win);
}

static ERL_NIF_TERM e_cbreak(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return done(env, cbreak());
}

static ERL_NIF_TERM e_nocbreak(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return done(env, nocbreak());
}

static ERL_NIF_TERM e_echo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return done(env, echo());
}

static ERL_NIF_TERM e_werase(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win;
    enif_get_long(env, argv[0], &win);
    return done(env, werase((WINDOW *)win));
}

static ERL_NIF_TERM e_noecho(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return done(env, noecho());
}

static ERL_NIF_TERM e_addch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long ch;
    enif_get_long(env, argv[0], &ch);
    return done(env, addch((chtype)ch));
}

static ERL_NIF_TERM e_addstr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long length;
    enif_get_long(env, argv[0], &length);
    char buff[length];
    enif_get_string(env, argv[1], buff, length+1, ERL_NIF_LATIN1);
    return done(env, addnstr(buff, length));
}

static ERL_NIF_TERM e_move(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long x, y;
    enif_get_long(env, argv[0], &x);
    enif_get_long(env, argv[1], &y);
    return done(env, move((int)y, (int)x));
}

static ErlNifFunc nif_funcs[] =
{
    {"refresh", 0, e_refresh},
    {"endwin", 0, e_endwin},
    {"initscr", 0, e_initscr},
    {"cbreak", 0, e_cbreak},
    {"nocbreak", 0, e_nocbreak},
    {"echo", 0, e_echo},
    {"noecho", 0, e_noecho},
    {"addch", 1, e_addch},
    {"e_addstr", 2, e_addstr},
    {"move", 2, e_move},
    {"werase", 1, e_werase}
};

ERL_NIF_INIT(encurses, nif_funcs, NULL, NULL, NULL, NULL)
