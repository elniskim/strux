#include <stdio.h>
#include <stdint.h>

#ifdef __cplusplus 
extern "C" {
#endif

void printInt (int64_t i) {
    printf("%ld", i);
}

void printFloat (double d) {
    printf("%f", d);
}

void printBool (int64_t b) {
    puts(b ? "True" : "False");
}

void printChar (int64_t c) {
    putchar((char)c);
}

void printStr (uint64_t *s) { 
    while (*s != 0) {
        putchar((char)*s);
        s++;
    }
}

#ifdef __cplusplus
}
#endif