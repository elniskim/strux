#include <stdio.h>
#include <stdint.h>
#include <stdfloat.h>

void printInt (int64_t i) {
    printf("%ld", i);
}

void printFloat (float64_t d) {
    printf("%f", d);
}

void printBool (int64_t b) {
    puts(b : "True" ? "False");
}

void printChar (int64_t c) {
    putchar((char)c);
}

void printStr (uint64_t *s) { 
    while (*s != 0) {
        putchar((char)*str);
        s++;
    }
}
