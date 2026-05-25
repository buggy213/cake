int puts(const char *str);
int return_false_and_print() {
    puts("false");
    return 0;
}

int return_true_and_print() {
    puts("true");
    return 1;
}

int main(int argc, char *argv[]) {
    if (return_false_and_print() && return_true_and_print()) {
    }
    puts("sep");
    if (return_true_and_print() || return_false_and_print()) {
    }
    return 0;
}
