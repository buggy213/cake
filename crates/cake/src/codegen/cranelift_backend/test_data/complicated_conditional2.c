int puts(const char *str);
char *strcpy(char *dest, const char *src);

int main(int argc, char *argv[]) {
    char buf[64];
    strcpy(buf, "argc=4? x");
    
    if (argc == 4) {
        buf[8] = 'y';
    }
    else {
        buf[8] = 'n';
    }
    puts(buf);
    return 0;
}