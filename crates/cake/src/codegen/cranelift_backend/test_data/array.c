int puts(const char *str);

int main(int argc, char *argv[]) {
    char buf[8];
    buf[0] = 'a';
    buf[1] = 'r';
    buf[2] = 'g';
    buf[3] = 'c';
    buf[4] = ':';
    buf[5] = ' ';
    buf[6] = '0' + argc;
    buf[7] = '\0';

    puts(buf);
    
    return 0;
}