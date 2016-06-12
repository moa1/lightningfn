all: lightningfn.h liblightningfn.so examples examplesfn

lightningfn.c: gen.py
	python gen.py > lightningfn.c

lightningfn.h: lightningfn.c head.lightningfn.h
#	cat lightningfn.c | grep "fn_jit_" | sed 's/ {/;/g' | sed -E 's/^(.*)/extern \1/g' > lightningfn.h.body
	cat lightningfn.c | grep "fn_jit_" | sed 's/ {/;/g' > lightningfn.h.body
	cat head.lightningfn.h lightningfn.h.body > lightningfn.h

#for linking to the shared library lightning.so: gcc -I ~/soft/lightning-2.1.0/include/ -L ~/soft/lightning-2.1.0/lib/.libs/ -llightning -shared -o lightningfn.so ./lightningfn.c
# on 64-bit, compile lightning with "./configure --with-pic", and compile lightningfn with --fPIC: gcc -fPIC -I ../lightning-2.1.0/include/ -shared -o liblightningfn.so ./lightningfn.c ../lightning-2.1.0/lib/.libs/liblightning.a
liblightningfn.so: lightningfn.c
	gcc -I ../lightning-2.1.0/include/ -shared -o liblightningfn.so ./lightningfn.c ../lightning-2.1.0/lib/.libs/liblightning.a

# this target is for when liblightning is compiled with --enable-disassembler
liblightningfn-disassembler.so: lightningfn.c
	gcc -I ../lightning-2.1.0/include/ -lbfd -lopcodes -shared -o liblightningfn.so ./lightningfn.c ../lightning-2.1.0/lib/.libs/liblightning.a

examples: examples.c lightningfn.h
	gcc -l lightning -L ../lightning-2.1.0/lib/.libs/ -I ../lightning-2.1.0/include/ -o examples examples.c

examplesfn: examplesfn.c lightningfn.h
	gcc -I . -L . -l lightningfn -o examplesfn examplesfn.c

examplesfn_indirect: examplesfn_indirect.c lightningfn.h
	gcc -I . -L . -l lightningfn -o examplesfn_indirect examplesfn_indirect.c

clean:
	rm lightningfn.h lightningfn.c liblightningfn.so lightningfn.h.body examples examplesfn
