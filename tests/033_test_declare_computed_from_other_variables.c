int main() {
    int a = 2;
    int b = a * 3;
    int c = 0;
    int d = a * b - (c = 2);
    return d + c == a * b;
}