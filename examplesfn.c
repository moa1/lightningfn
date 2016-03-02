// gcc -I . -L . -l lightningfn -o examplesfn examplesfn.c
// LD_LIBRARY_PATH=. ./examplesfn

#include <stdio.h>
#include <lightningfn.h>
#include <assert.h>
#include <time.h>

typedef int (*pifi)(int);    /* Pointer to Int Function of Int */

int incr(int x)
{
	jit_state_t *_jit;

	jit_node_t  *in;
	pifi         incr;
	
	_jit = fn_jit_new_state();

	jit_gpr_t r0 = fn_jit_r(0);
	
	fn_jit_prolog(_jit);
	in = fn_jit_arg(_jit);
	fn_jit_getarg(_jit, r0, in);
	fn_jit_addi(_jit, r0, r0, 1);
	fn_jit_retr(_jit, r0);
	fn_jit_epilog(_jit);
	
	incr = fn_jit_emit(_jit);
	fn_jit_clear_state(_jit); //this is needed, otherwise there is a memory leak
	
	/* call the generated code, passing x as an argument */
	int y = incr(x);
	
	fn_jit_destroy_state(_jit);

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
