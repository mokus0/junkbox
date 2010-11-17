
int main() {
    signed   char x = 0x7f;
    unsigned char y = 0xff;
    short z = x + y;
    
    printf("(signed char) %d + (unsigned char) %d -> (short) %d\n", (int) x, (int) y, (int) z);
    return 0;
}