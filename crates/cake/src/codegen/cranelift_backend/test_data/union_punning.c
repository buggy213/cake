int printf(const char *fmt, ...);
union data {
    long num;
    char c[8];
};

int main() {
    union data x;
    x.num = 0x0064636261;

    printf("x.c = %s", x.c);

    return 0;
}
