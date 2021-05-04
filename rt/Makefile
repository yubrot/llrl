LIB = libllrt.a
CC = clang
CFLAGS = -Wall -O2 -fPIC -I.
SRCS = $(wildcard *.c)
OBJS = $(patsubst %.c,%.o,$(SRCS))

all: $(LIB)

$(LIB): $(OBJS)
	ar rcs $(LIB) $(OBJS)

%.o: %.c
	$(CC) $(CFLAGS) -o $@ -c $<
