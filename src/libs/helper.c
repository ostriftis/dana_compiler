#include <stdio.h>
#include <string.h>

long long readInteger() {
    long long val;
    scanf("%lld", &val);
    return val;
}

void writeInteger(long long n) {
    printf("%lld", n);
}

char readByte() {
    return (char)getchar();
}

char readChar() {
    return (char)getchar();
}

void writeByte(char b) {
    printf("%d", (unsigned char)b);
}

void writeChar(char c) {
    putchar((int)c);
}

void readString(long long size, char *buffer) {
    fgets(buffer, (int)size, stdin);
    buffer[strcspn(buffer, "\n")] = 0;
}

void writeString(char *s) {
    printf("%s", s);
}

long long extend(char b) {
    return (long long)b;
}

char shrink(long long i) {
    return (char)i;
}
