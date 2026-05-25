int printf(const char *fmt, ...);
void my_strcpy(char *dest, const char *src) {
    while (*src) {
        *(dest++) = *(src++);
    }
    *dest = '\0';
}
int main(int argc, char *argv[]) {
    int i;
    int j;
    j = 1;
    for (i = 0; i < 10; i += 1) {
        j *= 2;
    }
    printf("%d ", j);

    int x;
    x = 10;
    printf("%d ", ++x);
    printf("%d ", x++);

    char buf[64];
    my_strcpy(buf, "test test");
    printf("%s", buf);

    return x;
}
