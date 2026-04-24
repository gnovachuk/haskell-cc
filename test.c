int foo(int a, int b) {
    print a + b;
}

int bar() {
    print 420;
}

int main() {
  print foo(34, 35) || bar();
  return 0;
}
