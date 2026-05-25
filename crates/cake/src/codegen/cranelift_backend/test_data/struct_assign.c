int printf(const char *fmt, ...);
struct data {
    int a;
    float b;
};

int main() {
    struct data x;
    x.a = 5;
    x.b = 2.0f;

    struct data y;
    y = x;
    printf("y.a = %d, y.b = %.1f\n", y.a, y.b);

    return 0;
}
