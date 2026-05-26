int printf(const char *fmt, ...);

int main() {
    int a = 0;
    int b = 1;
    while (a < 500) {
        int tmp = b;
        b = a + b;
        a = tmp;
        printf("%d ", a);
    }
    return 0;
}