int puts(const char *str);
char *gets(char *str);

int main() {
    char buf[16];
    gets(buf);

    int n;
    n = buf[0] - '0';

    switch (n) {
        case 1:
            puts("one!");
            break;
        case 2:
            puts("two!");
            break;
        case 3:
            puts("three!");
            break;
        default:
            puts("nope");
            break;
    }

    return 0;
}
