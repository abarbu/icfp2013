CSCFLAGS = -O2 
# -O4 -optimize-leaf-routines -u -unboxing  -no-argc-checks -no-bound-checks -no-procedure-checks -local
CSC = csc $(CSCFLAGS)

PACKAGES = medea
SOURCES = $(shell find src/ -iname \*.scm -or -iname \*.ss)
IGNORES = src/main.scm
NONEXEC_SOURCES = $(filter-out $(IGNORES), $(SOURCES))

all: setup main
main: $(addsuffix .o, $(basename $(NONEXEC_SOURCES))) src/main.o
	$(CSC) -o $@ $^

setup: setup
	# From https://bitbucket.org/Timusan/json-rpc/get/30baa663e046.zip
	unzip 30baa663e046.zip
	cd Timusan-json-rpc-30baa663e046/; chicken-install -s -l ./
	chicken-install -s $(PACKAGES)
	touch setup

%.o: %.scm
	$(CSC) -c $<

%.o: %.ss
	$(CSC) -c $<
