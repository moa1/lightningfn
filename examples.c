// gcc -l lightning -L ../lightning-2.1.0/lib/.libs/ -I ../lightning-2.1.0/include/ -o examples examples.c
// LD_LIBRARY_PATH=../lightning-2.1.0/lib/.libs/ ./examples

#include <stdio.h>
#include <lightning.h>
#include <assert.h>
#include <time.h>

static jit_state_t *_jit;

typedef int (*pifi)(int);    /* Pointer to Int Function of Int */

int incr(int x)
{
	jit_node_t  *in;
	pifi         incr;
	
	_jit = jit_new_state();
	
	jit_prolog();
	in = jit_arg();
	jit_getarg(JIT_R0, in);
	jit_addi(JIT_R0, JIT_R0, 1);
	jit_retr(JIT_R0);
	jit_epilog();
	
	incr = jit_emit();
	jit_clear_state(); //this is needed, otherwise there is a memory leak
	
	/* call the generated code, passing x as an argument */
	int y = incr(x);
	
	jit_destroy_state();

	return y;
}

int main(int argc, char *argv[])
{
	init_jit(argv[0]);

	int num_seconds=5;
	clock_t start=clock(), now;
	clock_t onesecond=CLOCKS_PER_SEC;
	
	int i,j;
	for (i=0;now = clock(),now-start<onesecond*num_seconds;) {
		for (j=0;j<1000;j++,i++) {
			assert(incr(5)==6);
		}
	}
	printf("iterations per second: %f\n", ((float)i)/(((float)now-start)/onesecond));

	// loop to be able to see in top how much memory the process uses
	while(1) {}
	
	finish_jit();
	return 0;
}
