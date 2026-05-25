int printf(const char *fmt, ...);
struct data {
    int a;
    float b;
};

int main() {
    struct data x;
    x.a = 5;
    x.b = 2.0f;

    printf("x.a = %d, x.b = %.1f\n", x.a, x.b);

    struct data *y;
    y = &x;
    y->a = 1;
    y->b = 42.0f;

    printf("y->a = %d, y->b = %.1f\n", y->a, y->b);

    return 0;
}
