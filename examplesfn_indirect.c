// gcc -I . -L . -l lightningfn -o examplesfn examplesfn.c
// LD_LIBRARY_PATH=. ./examplesfn

#include <stdio.h>
#include <lightningfn.h>
#include <assert.h>
#include <time.h>

typedef int (*pifi)();    /* Pointer to Int Function of Int */

int indirect(int par)
{
	jit_state_t *_jit;
	unsigned int store=0xabc;
	void* label1;
	void* label2;
	jit_node_t *node1;
	jit_node_t *node2;

	jit_node_t  *in;
	pifi         indirectfn;
	
	_jit = fn_jit_new_state();

	jit_gpr_t r0 = fn_jit_r(0);
	
	fn_jit_prolog(_jit);
	fn_jit_ldi(_jit, r0, &store); //load the value stored in `store` into `r0`
	//fn_jit_retr(_jit, r0);
	fn_jit_jmpr(_jit, r0); //jump to the address stored in `r0`.
	node1 = fn_jit_note(_jit, __FILE__, __LINE__);
	fn_jit_reti(_jit, 0);
	node2 = fn_jit_note(_jit, __FILE__, __LINE__);
	fn_jit_reti(_jit, 1);
	fn_jit_epilog(_jit);
	
	indirectfn = fn_jit_emit(_jit);

	label1=fn_jit_address(_jit, node1);
	label2=fn_jit_address(_jit, node2);
	printf("label1:%x label2:%x\n", label1, label2);

	fn_jit_disassemble(_jit);

	fn_jit_clear_state(_jit); //this is needed, otherwise there is a memory leak
	
	/* call the generated code, jumping to label1 if par==0, jumping to label2 else */
	if (par==0) {
		store=(unsigned int)label1;
	} else {
		store=(unsigned int)label2;
	}
	int y = indirectfn();
	printf("y:%x\n", y);
	
	fn_jit_destroy_state(_jit);

	return y;
}

int main(int argc, char *argv[])
{
	init_fn_jit(argv[0]);

	int num_seconds=5;
	clock_t start=clock(), now;
	clock_t onesecond=CLOCKS_PER_SEC;
	
	indirect(1);
	indirect(0);

	finish_fn_jit();
	return 0;
}
