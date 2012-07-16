#include <stdio.h>

// quick test of a claim I made in IRC that this wouldn't compile
// 
// Well, technically it does compile, but not to an executable...
// "gcc" creates an object file with no symbols, and "gcc -O"
// creates one without any code or data either.
static int main() {
	printf("You found me!\n");
}
