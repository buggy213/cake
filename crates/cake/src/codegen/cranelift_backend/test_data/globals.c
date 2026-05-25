int printf(const char *fmt, ...);
int counter;

int fetch_add() {
    int old_counter;
    old_counter = counter;
    counter = counter + 1;
    return old_counter;
}

int main(int argc, char *argv[]) {
    int x;
    x = fetch_add();
    printf("x: %d, ", x);
    x = fetch_add();
    printf("x: %d, ", x);
    return 0;
}
