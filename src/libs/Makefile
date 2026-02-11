CC      := clang
CFLAGS  := -Wall -O2 -std=gnu17
AR      := ar
ARFLAGS := rcs

all: lib.a

lib.a: helper.o
	$(AR) $(ARFLAGS) $@ $^

helper.o: helper.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f helper.o lib.a
