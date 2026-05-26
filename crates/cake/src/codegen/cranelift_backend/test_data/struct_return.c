int printf(const char *fmt, ...);

struct data {
    int a;
    float b;
};

struct data new_data() {
    struct data x;
    x.a = 1;
    x.b = 1.0f;
    return x;
}

int main() {
    struct data x = new_data();
    printf("x.a = %d, x.b = %.1f\n", x.a, x.b);
    return 0;
}
