int printf(const char *fmt, ...);
struct data {
    int a;
    float b;
};

void modify_and_print_data(struct data x) {
    x.a = 1;
    x.b = 1.0f;
    printf("x.a = %d, x.b = %.1f\n", x.a, x.b);
}

int main() {
    struct data x;
    x.a = 5;
    x.b = 2.0f;

    modify_and_print_data(x);
    printf("x.a = %d, x.b = %.1f\n", x.a, x.b);

    return 0;
}
