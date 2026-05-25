int printf(const char *fmt, ...);
struct data {
    int a;
    float b;
};

int main() {
    int i;
    float *p;
    char a[52];
    struct data s;
    struct data sa[18];

    printf("%d %d %d %d %d %d %d %d %d",
        sizeof(int),
        sizeof i,
        sizeof(void *),
        sizeof p,
        sizeof *p,
        sizeof (char[52]),
        sizeof a,
        sizeof (struct data),
        sizeof s,
        sizeof (struct data [18]),
        sizeof sa
    );

    return 0;
}
