all: lightningfn.so lightningfn.h

lightningfn.c: gen.py
	python gen.py > lightningfn.c

lightningfn.h: lightningfn.c head.lightningfn.h
#	cat lightningfn.c | grep "fn_jit_" | sed 's/ {/;/g' | sed -E 's/^(.*)/extern \1/g' > lightningfn.h.body
	cat lightningfn.c | grep "fn_jit_" | sed 's/ {/;/g' > lightningfn.h.body
	cat head.lightningfn.h lightningfn.h.body > lightningfn.h

#for linking to the shared library lightning.so: gcc -I ~/soft/lightning-2.1.0/include/ -L ~/soft/lightning-2.1.0/lib/.libs/ -llightning -shared -o lightningfn.so ./lightningfn.c
lightningfn.so: lightningfn.c
	gcc -I ../lightning-2.1.0/include/ -shared -o lightningfn.so ./lightningfn.c ../lightning-2.1.0/lib/.libs/liblightning.a

clean:
	rm lightningfn.h lightningfn.c lightningfn.so lightningfn.h.body
