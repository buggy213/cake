int puts(const char *str);
void itoa(int value, char *buf) {
    int value_copy;
    value_copy = value;

    int digits;
    digits = 0;

    do {
        value_copy = value_copy / 10;
        digits = digits + 1;
    } while (value_copy);

    buf[digits] = '\0';
    do {
        int lsd;
        lsd = value % 10;
        value = value / 10;
        buf[digits - 1] = '0' + lsd;
        digits = digits - 1;
    } while (value);
}

int main(int argc, char *argv[]) {
    int i;
    char buf[32];
    i = 0;
    while (1) {
        itoa(i, buf);
        if (i > 10) {
            break;
        }

        if (i % 2 == 1) {
            i = i + 1;
            continue;
        }

        puts(buf);
        i = i + 1;
    }

    for (i = 0;; i = i + 1) {
        itoa(i, buf);
        if (i > 10) {
            break;
        }

        if (i % 2 == 0) {
            continue;
        }

        puts(buf);
    }

    return 0;
}
