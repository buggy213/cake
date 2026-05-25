int puts(const char *str);
void my_strcpy_for(char *dest, char *src) {
    char *tmp;
    for (tmp = src; *tmp != '\0'; tmp = tmp + 1, dest = dest + 1) {
        *dest = *tmp;
    }
    *dest = '\0';
}

int main(int argc, char *argv[]) {
    char buf[32];

    my_strcpy_for(buf, "Hello world!");
    puts(buf);
    return 0;
}
