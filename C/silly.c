#include <stdio.h>

static char *i = "Hello World\n";

int main() {
	int a = 0, c;
	do { putchar(c = a++ [i]); } while (c);
	return c;
}