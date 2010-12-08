#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <signal.h>
#include <ncurses.h>
#include "erl_nif.h"
#include "encurses.h"

static ErlNifMutex* g_lock = NULL;

typedef struct _qitem_t
{
    struct _qitem_t* next;
    ErlNifPid* pid;
} qitem_t;

typedef struct
{
    ErlNifMutex* lock;
    ErlNifCond* cond;
    qitem_t* head;
    qitem_t* tail;
} queue_t;

typedef struct
{
    ErlNifThreadOpts* opts;
    ErlNifTid qthread;
    queue_t* queue;
    ERL_NIF_TERM atom_ok;
} state_t;

queue_t*
queue_create()
{
    queue_t* ret;
    
    ret = (queue_t*) enif_alloc(sizeof(queue_t));
    if(ret == NULL) return NULL;

    ret->lock = NULL; 
    ret->cond = NULL; 
    ret->head = NULL; 
    ret->tail = NULL;

    ret->lock = enif_mutex_create("q_lock");
    if(ret->lock == NULL) goto error;

    ret->cond = enif_cond_create("q_cond");
    if(ret->cond == NULL) goto error;

    return ret;

error:
    if(ret->lock != NULL) enif_mutex_destroy(ret->lock);
    if(ret->cond != NULL) enif_cond_destroy(ret->cond);
    if(ret != NULL) enif_free(ret);
    return NULL;
}

void
queue_destroy(queue_t* queue)
{
    ErlNifMutex* lock;
    ErlNifCond* cond;

    enif_mutex_lock(queue->lock);
    assert(queue->head == NULL && "Destroying a non-empty queue.");
    assert(queue->tail == NULL && "Destroying a queue in an invalid state.");
    lock = queue->lock;
    cond = queue->cond;
    queue->lock = NULL;
    queue->cond = NULL;
    enif_mutex_unlock(lock);

    enif_cond_destroy(cond);
    enif_mutex_destroy(lock);
    enif_free(queue);
}

int
queue_push(queue_t* queue, ErlNifPid* pid)
{
    qitem_t* item = (qitem_t*) enif_alloc(sizeof(qitem_t));
    if(item == NULL) return 0;

    item->pid = pid;
    item->next = NULL;

    enif_mutex_lock(queue->lock);

    if(queue->tail != NULL)
    {
        queue->tail->next = item;
    }

    queue->tail = item;

    if(queue->head == NULL)
    {
        queue->head = queue->tail;
    }

    enif_cond_signal(queue->cond);
    enif_mutex_unlock(queue->lock);

    return 1;
}

ErlNifPid*
queue_pop(queue_t* queue)
{
    qitem_t* item;
    ErlNifPid* ret = NULL;
    
    enif_mutex_lock(queue->lock);

    while(queue->head == NULL)
    {
        enif_cond_wait(queue->cond, queue->lock);
    }

    item = queue->head;
    queue->head = item->next;
    item->next = NULL;

    if(queue->head == NULL)
    {
        queue->tail = NULL;
    }

    enif_mutex_unlock(queue->lock);

    ret = item->pid;
    enif_free(item);

    return ret;
}

/** implementations **/

/* NIF management */

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    g_lock = enif_mutex_create("g_lock");

    int i;
    for(i=0;i<_MAXWINDOWS;i++){
        slots[i] = NULL;
    }

    state_t* state = (state_t*) enif_alloc(sizeof(state_t));
    if(state == NULL) return -1;

    state->queue = queue_create();
    if(state->queue == NULL) goto error;

    state->opts = enif_thread_opts_create("thread_opts");
    if(enif_thread_create("", &(state->qthread), thr_main, state, state->opts
                ) != 0)
    {
        goto error;
    }

    state->atom_ok = enif_make_atom(env, "ok");

    *priv = (void*) state;

    return 0;

error:
    if(g_lock != NULL) enif_mutex_destroy(g_lock);
    if(state->queue != NULL) queue_destroy(state->queue);
    enif_free(state->queue);
    return -1;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    state_t* state = (state_t*) priv;
    void* resp;

    if(g_lock != NULL) enif_mutex_destroy(g_lock);

    queue_push(state->queue, NULL);
    enif_thread_join(state->qthread, &resp);
    queue_destroy(state->queue);

    enif_thread_opts_destroy(state->opts);
    enif_free(state);
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
    enif_mutex_lock(g_lock);
    int code = refresh();
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

static ERL_NIF_TERM 
e_wrefresh(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win;
    enif_get_long(env, argv[0], &win);

    enif_mutex_lock(g_lock);
    int code = wrefresh(slots[win]);
    enif_mutex_unlock(g_lock);

    return done(env, code);
}

// newwin

static ERL_NIF_TERM 
e_newwin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enif_mutex_lock(g_lock);
    int slot = find_free_window_slot();
    if(slot >= 1) {
        int width, height, startx, starty;
        enif_get_int(env, argv[0], &width);
        enif_get_int(env, argv[1], &height);
        enif_get_int(env, argv[2], &startx);
        enif_get_int(env, argv[3], &starty);
        slots[slot] = newwin(height, width, starty, startx);
        enif_mutex_unlock(g_lock);
        return enif_make_long(env, slot);
    } else {
        enif_mutex_unlock(g_lock);
        return done(env, FALSE);
    }
}

// delwin

static ERL_NIF_TERM 
e_delwin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long slot;
    enif_get_long(env, argv[0], &slot);
    enif_mutex_lock(g_lock);
    if(slot == 0) 
    {
        enif_mutex_unlock(g_lock);
        return done(env, FALSE);
    }
    else if(slots[slot] == NULL) 
    {
        enif_mutex_unlock(g_lock);
        return done(env, FALSE);
    }
    else 
    {
        delwin(slots[slot]);
        slots[slot] = NULL;
        enif_mutex_unlock(g_lock);
        return done(env, OK);
    }
}

// endwin

static ERL_NIF_TERM 
e_endwin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enif_mutex_lock(g_lock);
    int code = endwin();
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

// initscr

static ERL_NIF_TERM 
e_initscr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enif_mutex_lock(g_lock);
    slots[0] = (WINDOW *)initscr();
    enif_mutex_unlock(g_lock);
    return enif_make_long(env, 0);
}

// cbreak

static ERL_NIF_TERM 
e_cbreak(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enif_mutex_lock(g_lock);
    int code = cbreak();
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

// nocbreak

static ERL_NIF_TERM 
e_nocbreak(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enif_mutex_lock(g_lock);
    int code = nocbreak();
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

// echo

static ERL_NIF_TERM 
e_echo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enif_mutex_lock(g_lock);
    int code = echo();
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

// noecho

static ERL_NIF_TERM 
e_noecho(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enif_mutex_lock(g_lock);
    int code = noecho();
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

// erase

static ERL_NIF_TERM 
int_erase(ErlNifEnv* env, WINDOW *win)
{
    enif_mutex_lock(g_lock);
    int code = werase(win);
    enif_mutex_unlock(g_lock);
    return done(env, code);
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
    enif_mutex_lock(g_lock);
    int code = addch((chtype)ch);
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

static ERL_NIF_TERM 
e_waddch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win, ch;
    enif_get_long(env, argv[0], &win);
    enif_get_long(env, argv[1], &ch);
    enif_mutex_lock(g_lock);
    int code = waddch(slots[win], (chtype)ch);
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

static ERL_NIF_TERM 
e_mvaddch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long x, y, ch;
    enif_get_long(env, argv[0], &x);
    enif_get_long(env, argv[1], &y);
    enif_get_long(env, argv[2], &ch);
    enif_mutex_lock(g_lock);
    int code = mvaddch((int)y, (int)x, (chtype)ch);
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

static ERL_NIF_TERM 
e_mvwaddch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win, x, y, ch;
    enif_get_long(env, argv[0], &win);
    enif_get_long(env, argv[1], &x);
    enif_get_long(env, argv[2], &y);
    enif_get_long(env, argv[3], &ch);
    enif_mutex_lock(g_lock);
    int code = mvwaddch(slots[win], (int)y, (int)x, (chtype)ch);
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

// addstr

static ERL_NIF_TERM
e_addstr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long length;
    enif_get_long(env, argv[0], &length);
    char buff[length];
    enif_get_string(env, argv[1], buff, length+1, ERL_NIF_LATIN1);
    enif_mutex_lock(g_lock);
    int code = addnstr(buff, length);
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

static ERL_NIF_TERM 
e_waddstr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int length;
    enif_get_int(env, argv[1], &length);

    char buff[length];
    enif_get_string(env, argv[2], buff, length+1, ERL_NIF_LATIN1);

    int win;
    enif_get_int(env, argv[0], &win);
    
    enif_mutex_lock(g_lock);
    int code = waddnstr(slots[win], buff, length);
    enif_mutex_unlock(g_lock);

    return done(env, code);
}

static ERL_NIF_TERM 
e_mvaddstr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int length;
    enif_get_int(env, argv[2], &length);

    char buff[length];
    enif_get_string(env, argv[3], buff, length+1, ERL_NIF_LATIN1);

    int x, y;
    enif_get_int(env, argv[0], &x);
    enif_get_int(env, argv[1], &y);
    
    enif_mutex_lock(g_lock);
    int code = mvaddnstr(y, x, buff, length);
    enif_mutex_unlock(g_lock);

    return done(env, code);
}

static ERL_NIF_TERM 
e_mvwaddstr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int length;
    enif_get_int(env, argv[3], &length);

    char buff[length];
    enif_get_string(env, argv[4], buff, length+1, ERL_NIF_LATIN1);

    int win, x, y;
    enif_get_int(env, argv[0], &win);
    enif_get_int(env, argv[1], &x);
    enif_get_int(env, argv[2], &y);

    enif_mutex_lock(g_lock);
    int code = mvwaddnstr(slots[win], y, x, buff, length);
    enif_mutex_unlock(g_lock);

    return done(env, code);
}

// move

static ERL_NIF_TERM
e_move(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int x, y;
    enif_get_int(env, argv[0], &x);
    enif_get_int(env, argv[1], &y);
    enif_mutex_lock(g_lock);
    int code = move(y,x);
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

static ERL_NIF_TERM
e_wmove(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int win, x, y;
    enif_get_int(env, argv[0], &win);
    enif_get_int(env, argv[1], &x);
    enif_get_int(env, argv[2], &y);
    enif_mutex_lock(g_lock);
    int code = wmove(slots[win], y,x);
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

// getxy

static ERL_NIF_TERM
int_getxy(ErlNifEnv* env, WINDOW *win)
{
    long x, y;
    enif_mutex_lock(g_lock);
    getyx(win, y, x);
    enif_mutex_unlock(g_lock);
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
    enif_mutex_lock(g_lock);
    getmaxyx(win, y, x);
    enif_mutex_unlock(g_lock);
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
    enif_mutex_lock(g_lock);
    int code = curs_set((int)flag);
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

// has_colors

static ERL_NIF_TERM 
e_has_colors(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enif_mutex_lock(g_lock);
    int code = has_colors();
    enif_mutex_unlock(g_lock);
    return boolean(env, code);
}

// start_colors

static ERL_NIF_TERM 
e_start_color(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enif_mutex_lock(g_lock);
    int code = start_color();
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

// init_pair

static ERL_NIF_TERM
e_init_pair(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long n, fcolor, bcolor;
    enif_get_long(env, argv[0], &n);
    enif_get_long(env, argv[1], &fcolor);
    enif_get_long(env, argv[2], &bcolor);

    enif_mutex_lock(g_lock);
    int code = init_pair((int)n, (int)fcolor, (int)bcolor);
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

// attron/attroff

static ERL_NIF_TERM 
e_attron(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long attrs;
    enif_get_long(env, argv[0], &attrs);
    enif_mutex_lock(g_lock);
    int code = wattron(slots[0], (int)attrs);
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

static ERL_NIF_TERM 
e_wattron(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win, attrs;
    enif_get_long(env, argv[0], &win);
    enif_get_long(env, argv[1], &attrs);
    enif_mutex_lock(g_lock);
    int code = wattron(slots[win], (int)attrs);
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

static ERL_NIF_TERM 
e_attroff(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long attrs;
    enif_get_long(env, argv[0], &attrs);
    enif_mutex_lock(g_lock);
    int code = wattroff(slots[0], (int)attrs);
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

static ERL_NIF_TERM 
e_wattroff(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win, attrs;
    enif_get_long(env, argv[0], &win);
    enif_get_long(env, argv[1], &attrs);
    enif_mutex_lock(g_lock);
    int code = wattroff(slots[win], (int)attrs);
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

// nl/nonl

static ERL_NIF_TERM 
e_nl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enif_mutex_lock(g_lock);
    int code = nl();
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

static ERL_NIF_TERM 
e_nonl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enif_mutex_lock(g_lock);
    int code = nonl();
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

// scrollok

static ERL_NIF_TERM 
e_scrollok(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win, flag;
    enif_get_long(env, argv[0], &win);
    enif_get_long(env, argv[1], &flag);
    enif_mutex_lock(g_lock);
    int code = scrollok(slots[win], (int)flag);
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

// vline/hline

static ERL_NIF_TERM 
e_hline(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long ch, max;
    enif_get_long(env, argv[0], &ch);
    enif_get_long(env, argv[1], &max);
    enif_mutex_lock(g_lock);
    int code = whline(slots[0], (chtype)ch, (int)max);
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

static ERL_NIF_TERM 
e_whline(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win, ch, max;
    enif_get_long(env, argv[0], &win);
    enif_get_long(env, argv[1], &ch);
    enif_get_long(env, argv[2], &max);
    enif_mutex_lock(g_lock);
    int code = whline(slots[win], (chtype)ch, (int)max);
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

static ERL_NIF_TERM 
e_vline(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long ch, max;
    enif_get_long(env, argv[0], &ch);
    enif_get_long(env, argv[1], &max);
    enif_mutex_lock(g_lock);
    int code = wvline(slots[0], (chtype)ch, (int)max);
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

static ERL_NIF_TERM 
e_wvline(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win, ch, max;
    enif_get_long(env, argv[0], &win);
    enif_get_long(env, argv[1], &ch);
    enif_get_long(env, argv[2], &max);
    enif_mutex_lock(g_lock);
    int code = wvline(slots[win], (chtype)ch, (int)max);
    enif_mutex_unlock(g_lock);
    return done(env, code);
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
    enif_mutex_lock(g_lock);
    int code = wborder(slots[0], (chtype)ls, (chtype)rs, (chtype)ts,
            (chtype)bs, (chtype)tl, (chtype)tr, (chtype)bl, (chtype)br);
    enif_mutex_unlock(g_lock);
    return done(env, code);
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
    enif_mutex_lock(g_lock);
    int code = wborder(slots[win], (chtype)ls, (chtype)rs, (chtype)ts,
            (chtype)bs, (chtype)tl, (chtype)tr, (chtype)bl, (chtype)br);
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

// box

static ERL_NIF_TERM 
e_box(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win, hch, vch;
    enif_get_long(env, argv[0], &win);
    enif_get_long(env, argv[1], &hch);
    enif_get_long(env, argv[2], &vch);
    enif_mutex_lock(g_lock);
    int code = box(slots[win], (chtype)vch, (chtype)hch);
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

// keypad

static ERL_NIF_TERM 
e_keypad(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long win, flag;
    enif_get_long(env, argv[0], &win);
    enif_get_long(env, argv[1], &flag);
    enif_mutex_lock(g_lock);
    int code = keypad(slots[win], (int)flag);
    enif_mutex_unlock(g_lock);
    return done(env, code);
}

// getch

static ERL_NIF_TERM 
e_getch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    state_t* state = (state_t*) enif_priv_data(env);
    ErlNifPid* pid = (ErlNifPid*) enif_alloc(sizeof(ErlNifPid));

    if(!enif_get_local_pid(env, argv[0], pid))
    {
        return enif_make_badarg(env);
    }

    queue_push(state->queue, pid);

    return state->atom_ok;
}

static void*
thr_main(void* obj)
{
    state_t* state = (state_t*) obj;
    ErlNifEnv* env = enif_alloc_env();
    ErlNifPid* pid;
    ERL_NIF_TERM msg;

    while((pid = queue_pop(state->queue)) != NULL)
    {
        msg = enif_make_int(env, getch());
        enif_send(NULL, pid, env, msg);
        enif_free(pid);
        enif_clear_env(env);
    }

    return NULL;
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
    {"e_getch", 1, e_getch},
};

ERL_NIF_INIT(encurses, nif_funcs, load, NULL, NULL, unload)
