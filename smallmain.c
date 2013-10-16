#include <stdio.h>
#include <dlfcn.h>

void test(void) {
    void *dl = dlopen("/home/ollie/work/plhaskell/plhaskell.so", RTLD_LAZY);
    printf("%d\n", ((int(*)(void)) dlsym(dl, "test"))());
    dlclose(dl);
}

int main(int argc, char *argv[]) {
    test(); test(); test();
    return 0;
}
