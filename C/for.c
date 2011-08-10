// 'for' is a higher-order function.

#include <setjmp.h>

#ifndef NULL
#  define NULL ((void *) 0)
#endif

typedef void (*init_t)      (void *);
typedef int  (*test_t)      (void *);
typedef void (*advance_t)   (void *);
typedef struct continue_t_env *continue_t_env;
typedef void (*continue_t)  (const continue_t_env);
typedef void (*action_t)    (continue_t, const continue_t_env, void *);

// this part wouldn't be in the .h, so that 'env' is totally opaque to the action.
struct continue_t_env {
    jmp_buf env;
};

void continue_(const continue_t_env env) {
    longjmp(env->env, 1);
}

// This is, of course, incredibly clunky to use.  But, it shows how the "for loop"
// is actually just a higher-order function that is baked-in to the language.
// The same can be said of pretty much every other control flow construct.
void for_(void *state, init_t init, test_t test, advance_t advance, action_t act) {
    struct continue_t_env env = {};
    
    if (init != NULL) init(state);
    
    top:
    if (!test(state) || setjmp(env.env)) goto bottom;
    act(&continue_, &env, state);
    
    advance(state);
    
    goto top;
    
    bottom:
    return;
}
