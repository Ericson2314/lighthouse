PROGS = upload.cgi printinput.cgi redirect.cgi hello.cgi \
        cookie.cgi error.cgi showAllInputs.cgi

GHCFLAGS = -package cgi -fwarn-unused-imports -fallow-overlapping-instances

.PHONY: all clean

all: $(PROGS)

%.cgi: %.hs
	ghc $(GHCFLAGS) --make -o $@ $^ 

clean:
	-rm -f *.hi *.o
	-rm -f $(PROGS)