#include <stdio.h>

int main() {
    printf("sizeof(long double) = %d\n", sizeof(long double));
    
    long double eps = 1.0l;
    unsigned i = 0;
    while (1.0l + eps != 1.0l) { eps /= 2.0l; i++; }
    eps *= 2; 
    printf("eps = %Lg, i = %u\n", eps, i);
    
    long double big = 1.0l;
    long double tmp = big;
    i = 0;
    
    do {
        big = tmp;
        tmp = big * 2.0l;
        i++;
    } while (tmp / 2.0l == big);
    printf("big = %Lg, i = %u\n", big, i);

    long double tiny = 1.0l;
    tmp = tiny;
    i = 0;
    
    do {
        tiny = tmp;
        tmp = tiny * 0.5l;
        i++;
    } while (tmp * 2.0l == tiny);
    printf("tiny = %Lg, i = %u\n", tiny, i);
    
    return 0;
}