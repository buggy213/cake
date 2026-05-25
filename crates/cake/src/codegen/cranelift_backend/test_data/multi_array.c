int main() {
    int arr[2][3];
    arr[0][0] = 1;
    arr[0][1] = 2;
    arr[0][2] = 3;

    int (*pointer_to_first)[3] = &arr[0];
    int (*pointer_to_second)[3] = pointer_to_first + 1;
    (*pointer_to_second)[0] = 4;

    int *flattened = &arr[0][0];
    int sum = 0;
    for (int i = 0; i < 6; i++) {
        sum += flattened[i];
    }

    return sum;
}
