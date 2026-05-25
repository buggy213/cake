int puts(const char *str);
void my_strcpy(char *dest, const char *src) {
    while (*src) {
        *dest = *src;
        dest = dest + 1;
        src = src + 1;
    }

    *dest = *src;
}

int main(int argc, char *argv[]) {
    char buf[32];
    my_strcpy(buf, "hello from my_strcpy");
    puts(buf);
    return 0;
}
