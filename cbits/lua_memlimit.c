/*
** Memory limit for the Lua toolbox: a lua_Alloc that counts the bytes a Lua
** state holds and refuses to grow past a limit.
**
** Refusing an allocation makes Lua raise a memory error, which is a longjmp
** to the nearest protected call. That is only sound when no Haskell frame
** sits between the allocation and that protected call, so the allocator
** refuses only
**
**   - while `enforce` is set (the Haskell side sets it around the pcall that
**     runs the user script, not during set-up or result marshalling), and
**   - while no Haskell function is running (`hs_depth == 0`).
**
** Allocations made by a Haskell function are let through and accounted for;
** the wrapper around the Haskell call raises the error once the function has
** returned, from C, where the longjmp is safe.
*/
#include <stdlib.h>
#include <lua.h>
#include <lauxlib.h>

/* From the `lua` package (hslua's C glue). */
#define HSLUA_HSFUN_NAME "HsLuaFunction"
void hslua_registerhsfunmetatable(lua_State *L);
int hslua_callhsfun(lua_State *L);

typedef struct {
  lua_Alloc orig;
  void *orig_ud;
  size_t used;
  size_t limit;
  int enforce;
  int hs_depth;
  int exceeded;
} agents_memlimit;

static void *agents_memlimit_alloc(void *ud, void *ptr, size_t osize, size_t nsize)
{
  agents_memlimit *m = (agents_memlimit *)ud;
  /* When ptr is NULL, osize encodes the kind of object, not a size. */
  size_t old = ptr == NULL ? 0 : osize;

  if (nsize > old) {
    size_t grow = nsize - old;
    if (m->used > m->limit || grow > m->limit - m->used) {
      if (m->enforce && m->hs_depth == 0) {
        m->exceeded = 1;
        return NULL;
      }
    }
  }

  void *res = m->orig(m->orig_ud, ptr, osize, nsize);
  if (nsize == 0) {
    m->used = m->used >= old ? m->used - old : 0;
  } else if (res != NULL) {
    m->used = (m->used >= old ? m->used - old : 0) + nsize;
  }
  return res;
}

static agents_memlimit *agents_memlimit_get(lua_State *L)
{
  void *ud = NULL;
  if (lua_getallocf(L, &ud) != agents_memlimit_alloc) {
    return NULL;
  }
  return (agents_memlimit *)ud;
}

/*
** Replacement for the `__call` metamethod of Haskell function wrappers:
** runs the Haskell function with the hard limit suspended, then checks the
** limit on the way back.
*/
static int agents_memlimit_callhsfun(lua_State *L)
{
  agents_memlimit *m = agents_memlimit_get(L);
  if (m == NULL) {
    return hslua_callhsfun(L);
  }

  m->hs_depth++;
  int nres = hslua_callhsfun(L);
  m->hs_depth--;

  if (m->enforce && m->hs_depth == 0 && m->used > m->limit) {
    lua_gc(L, LUA_GCCOLLECT, 0);
    if (m->used > m->limit) {
      m->exceeded = 1;
      return luaL_error(L, "not enough memory");
    }
  }
  return nres;
}

/*
** Installs the limit on a state. Returns 1 on success, 0 when the tracking
** structure could not be allocated. Installing twice only updates the limit.
*/
int agents_memlimit_install(lua_State *L, size_t limit_bytes)
{
  agents_memlimit *m = agents_memlimit_get(L);
  if (m != NULL) {
    m->limit = limit_bytes;
    return 1;
  }

  m = (agents_memlimit *)malloc(sizeof(agents_memlimit));
  if (m == NULL) {
    return 0;
  }

  /* Route Haskell function calls through the depth-tracking wrapper. */
  if (luaL_getmetatable(L, HSLUA_HSFUN_NAME) != LUA_TTABLE) {
    /* Not a state made by hslua's newstate: create the metatable. The
     * registration only balances the stack when it creates the table. */
    lua_pop(L, 1);
    hslua_registerhsfunmetatable(L);
    luaL_getmetatable(L, HSLUA_HSFUN_NAME);
  }
  lua_pushcfunction(L, &agents_memlimit_callhsfun);
  lua_setfield(L, -2, "__call");
  lua_pop(L, 1);

  m->orig = lua_getallocf(L, &m->orig_ud);
  m->limit = limit_bytes;
  m->enforce = 0;
  m->hs_depth = 0;
  m->exceeded = 0;
  /* Start from what the state already holds, so frees stay balanced. */
  m->used = (size_t)lua_gc(L, LUA_GCCOUNT, 0) * 1024
          + (size_t)lua_gc(L, LUA_GCCOUNTB, 0);
  lua_setallocf(L, agents_memlimit_alloc, m);
  return 1;
}

/* Turns hard enforcement on or off; no-op on a state without a limit. */
void agents_memlimit_enforce(lua_State *L, int on)
{
  agents_memlimit *m = agents_memlimit_get(L);
  if (m != NULL) {
    m->enforce = on;
  }
}

/* Whether an allocation was ever refused on this state. */
int agents_memlimit_exceeded(lua_State *L)
{
  agents_memlimit *m = agents_memlimit_get(L);
  return m != NULL && m->exceeded;
}

/* Closes the state and releases the tracking structure, if any. */
void agents_memlimit_close(lua_State *L)
{
  agents_memlimit *m = agents_memlimit_get(L);
  lua_close(L);
  free(m);
}
