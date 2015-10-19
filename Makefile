all: lightningfn.so

lightningfn.c: gen.py
	python gen.py > lightningfn.c

#for linking to the shared library lightning.so: gcc -I ~/soft/lightning-2.1.0/include/ -L ~/soft/lightning-2.1.0/lib/.libs/ -llightning -shared -o lightningfn.so ./lightningfn.c
lightningfn.so: lightningfn.c
	gcc -I ../lightning-2.1.0/include/ -shared -o lightningfn.so ./lightningfn.c ../lightning-2.1.0/lib/.libs/liblightning.a
