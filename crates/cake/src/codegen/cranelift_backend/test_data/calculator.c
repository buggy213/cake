int puts(const char *str);
char *gets(char *str);
char *strcpy(char *dest, const char *src);
int strcmp(const char *str1, const char *str2);
int atoi(const char *str);
unsigned long strlen(const char *str);

int add(int a, int b) {
    return a + b;
}
int subtract(int a, int b) {
    return a - b;
}
int multiply(int a, int b) {
    return a * b;
}
int divide(int a, int b) {
    return a / b;
}

int main(int argc, char *argv[]) {
    char buf[32];
    int a;
    int b;
    puts("Enter first number: ");
    gets(buf);
    a = atoi(buf);
    puts("Enter second number: ");
    gets(buf);
    b = atoi(buf);

    int (*calc)(int, int);

    puts("Choose operation (+, -, *, /): ");
    gets(buf);

    if (strcmp(buf, "+") == 0) {
        calc = &add;
    }
    else if (strcmp(buf, "-") == 0) {
        calc = &subtract;
    }
    else if (strcmp(buf, "*") == 0) {
        calc = &multiply;
    }
    else if (strcmp(buf, "/") == 0) {
        calc = &divide;
    }
    else {
        return 0 - 1;
    }

    int answer;
    answer = calc(a, b);

    unsigned long idx;
    strcpy(buf, "The answer is: ");
    idx = strlen(buf);

    int answer_copy;
    answer_copy = answer;
    int digits;
    digits = 0;
    do {
        answer_copy = answer_copy / 10;
        digits = digits + 1;
    } while (answer_copy);

    buf[idx + digits] = '\0';
    do {
        int lsd;
        lsd = answer % 10;
        answer = answer / 10;
        buf[idx + digits - 1] = '0' + lsd;
        digits = digits - 1;
    } while (answer);

    puts(buf);

    return 0;
}
