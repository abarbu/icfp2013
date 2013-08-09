CSCFLAGS = -O2 
# -O4 -optimize-leaf-routines -u -unboxing  -no-argc-checks -no-bound-checks -no-procedure-checks -local
CSC = csc $(CSCFLAGS)
CC = gcc -Wall -Werror

PACKAGES = medea http-client
SOURCES = $(shell find src/ -iname \*.scm -or -iname \*.ss)
IGNORES = src/main.scm src/stuff.scm
NONEXEC_SOURCES = $(filter-out $(IGNORES), $(SOURCES))

all: setup main
main: $(addsuffix .o, $(basename $(NONEXEC_SOURCES))) src/main.o src/hobostuff_c.o
	$(CSC) -o $@ $^

setup: setup
	chicken-install -s $(PACKAGES)
	touch setup

%.o: %.scm
	$(CSC) -c $<

%.o: %.ss
	$(CSC) -c $<

%.o: %.c
	$(CC) -c $< -o $@