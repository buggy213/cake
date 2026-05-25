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
    char buf[16];
    int sum;
    sum = 0;

    int i, j;
    for (i = 0; i < 10; i = i + 1) {
        if (i % 2 == 0) {
            continue;
        }
        for (j = 0; j < 10; j = j + 1) {
            if (j % 2 == 0) {
                continue;
            }
            sum = sum + i * 10 + j;
        }
    }

    itoa(sum, buf);
    puts(buf);
    return 0;
}
