#include <stdio.h>

typedef __float128 quad;

int main() {
    printf("sizeof(quad) = %d\n", sizeof(quad));
    
    quad eps = 1.0l;
    unsigned i = 0;
    while (1.0l + eps != 1.0l) { eps /= 2.0l; i++; }
    eps *= 2; 
    printf("eps = %Lg, i = %u\n", (long double) eps, i);
    
    quad big = 1.0l;
    quad tmp = big;
    i = 0;
    
    do {
        big = tmp;
        tmp = big * 2.0l;
        i++;
    } while (tmp / 2.0l == big);
    printf("big = %Lg, i = %u\n", (long double) big, i);

    quad tiny = 1.0l;
    tmp = tiny;
    i = 0;
    
    do {
        tiny = tmp;
        tmp = tiny * 0.5l;
        i++;
    } while (tmp * 2.0l == tiny);
    printf("tiny = %Lg, i = %u\n", (long double) tiny, i);
    
    return 0;
}