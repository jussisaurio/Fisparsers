int main() {
    int a = 1;
    int b = 2;
    int c = a > 1 ? 2 : b == 2 ? 100 : 1;
    return c == 100;
}