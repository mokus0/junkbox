#import <stdio.h>

// arity-based macro overloading, how fun!
#define by_arity(macro, args...)    dispatch_by_arity(macro , ## args, 5, 4, 3, 2, 1, 0)(args)
#define dispatch_by_arity(macro,_1,_2,_3,_4,_5,N,...) macro ## _ ## N

#define trace(args...)              by_arity(trace, args)

#define trace_0()                   printf("\n")
#define trace_1(stmt)               printf("%-80s ==> ", #stmt); fflush(stdout); stmt; printf("%12s\n", "OK"); fflush(stdout)
#define trace_2(fmt, stmt)          printf("%-80s ==> %12" #fmt "\n", #stmt, (stmt)); fflush(stdout)
