int fib(int n) {
    if (n - 1) {
        if (n) return fib(n-1) + fib(n-2);
        else return 1;
    } else {
        return 1;
    }
}

int main() {
    int i = 0 - 10;
    int neg = 0 - 1;
    while (i) {
        int n = i + 10;
        print fib(n);
        i = i + 1;
    }
    return 0;
}
