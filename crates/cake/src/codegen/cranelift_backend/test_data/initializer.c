int global_inited = 2;
int main(int argc, char *argv[]) {
    int inited = 7;

    for (int loop_init = 3; loop_init > 0; loop_init += 1) {
        return global_inited + inited + loop_init;
    }

    return 0;
}
