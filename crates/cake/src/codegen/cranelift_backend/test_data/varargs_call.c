int printf(const char *fmt, ...);
int main(int argc, char *argv[]) {
    const char *hello;
    hello = "hello!";
    printf("argc: %d, %s", argc, hello);
    return 0;
}
