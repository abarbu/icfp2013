CSCFLAGS = -O2
# -O4 -optimize-leaf-routines -u -unboxing  -no-argc-checks -no-bound-checks -no-procedure-checks -local
CSC = csc $(CSCFLAGS)
CC = gcc -Wall -Werror

PACKAGES = medea http-client bind kvlists scheme2c-compatibility nondeterminism traversal random-bsd
SOURCES = $(shell find src/ -iname \*.scm -or -iname \*.ss)
IGNORES = src/main.scm src/master.scm src/slave.scm src/update.scm
NONEXEC_SOURCES = $(filter-out $(IGNORES), $(SOURCES))

all: setup main master slave
main: $(addsuffix .o, $(basename $(NONEXEC_SOURCES))) src/main.o
	$(CSC) -o $@ $^
master: $(addsuffix .o, $(basename $(NONEXEC_SOURCES))) src/master.o
	$(CSC) -o $@ $^
slave: $(addsuffix .o, $(basename $(NONEXEC_SOURCES))) src/slave.o
	$(CSC) -o $@ $^
update: $(addsuffix .o, $(basename $(NONEXEC_SOURCES))) src/update.o
	$(CSC) -o $@ $^
everything:
	csc -O3 -optimize-leaf-routines -u -unboxing  -no-argc-checks -no-bound-checks -no-procedure-checks -local everything.scm -o slave

setup: setup
	chicken-install -s $(PACKAGES)
	cd stuff && chicken-install -s -l ./
	touch setup

%.o: %.scm
	$(CSC) -c $<

%.o: %.ss
	$(CSC) -c $<

%.o: %.c
	$(CC) -c $< -o $@
lightning_package.tar: $(SOURCES)
	tar -cf $@ $(SOURCES) 
package.tar: $(SOURCES)
	tar -cf $@ $(SOURCES) 