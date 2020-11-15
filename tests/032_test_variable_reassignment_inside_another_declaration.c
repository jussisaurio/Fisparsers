int main() {
    int a = 1;
    int b = (a = 3) + 1;
    return b - a;
}