#!/usr/bin/python

print """#include "lightning.h"

jit_gpr_t fn_jit_r(jit_int8_t index) {
	return JIT_R(index);
}
jit_gpr_t fn_jit_v(jit_int8_t index) {
	return JIT_V(index);
}
jit_fpr_t fn_jit_f(jit_int8_t index) {
	return JIT_F(index);
}
jit_int8_t fn_jit_r_num() {
	return JIT_R_NUM;
}
jit_int8_t fn_jit_v_num() {
	return JIT_V_NUM;
}
jit_int8_t fn_jit_f_num() {
	return JIT_F_NUM;
}

/*
 * Prototypes
 */
void init_fn_jit(const char *progname) {
	init_jit(progname);
}
void finish_fn_jit(void) {
	finish_jit();
}
jit_state_t *fn_jit_new_state(void) {
	return jit_new_state();
}
void fn_jit_clear_state(jit_state_t *_jit) {
	_jit_clear_state(_jit);
}
void fn_jit_destroy_state(jit_state_t *_jit) {
	_jit_destroy_state(_jit);
}

jit_pointer_t fn_jit_address(jit_state_t *_jit, jit_node_t *node) {
	return _jit_address(_jit, node);
}
jit_node_t *fn_jit_name(jit_state_t *_jit, const char *name) {
	return _jit_name(_jit, name);
}
jit_node_t *fn_jit_note(jit_state_t *_jit, const char *name, int line) {
	return _jit_note(_jit, name, line);
}
jit_node_t *fn_jit_label(jit_state_t *_jit) {
	return _jit_label(_jit);
}
jit_node_t *fn_jit_forward(jit_state_t *_jit) {
	return _jit_forward(_jit);
}
jit_node_t *fn_jit_indirect(jit_state_t *_jit) {
	return _jit_indirect(_jit);
}
void fn_jit_link(jit_state_t *_jit, jit_node_t *node) {
	return _jit_link(_jit, node);
}
jit_bool_t fn_jit_forward_p(jit_state_t *_jit, jit_node_t *u) {
	return _jit_forward_p(_jit, u);
}
jit_bool_t fn_jit_indirect_p(jit_state_t *_jit,jit_node_t *u) {
	return _jit_indirect_p(_jit, u);
}
jit_bool_t fn_jit_target_p(jit_state_t *_jit, jit_node_t *u) {
	return _jit_target_p(_jit, u);
}

void fn_jit_prolog(jit_state_t *_jit) {
	_jit_prolog(_jit);
}

jit_int32_t fn_jit_allocai(jit_state_t *_jit, jit_int32_t length) {
	return _jit_allocai(_jit, length);
}
"""

def jit_getarg(name,type,u_type):
	print """
void fn_jit_getarg"""+name+"""(jit_state_t *_jit, """+u_type+""" u, jit_node_t *v) {
	_jit_getarg"""+type+"""(_jit, u, v);
}"""

for type in ["_c","_uc","_s","_us","_i"]:
	jit_getarg(type,type,"jit_gpr_t")
for type in ["_f","_d"]:
	jit_getarg(type,type,"jit_fpr_t")
print """
#if __WORDSIZE == 32"""
jit_getarg("","_i","jit_gpr_t")
print """
#else"""
for type in ["_ui","_l"]:
	jit_getarg(type,type,"jit_gpr_t")
jit_getarg("","_l","jit_gpr_t")
print """
#endif"""


def jit_putargr(name,type,u_type):
	print """
void fn_jit_putargr"""+name+"""(jit_state_t *_jit, """+u_type+""" u, jit_node_t *v) {
	_jit_putargr"""+type+"""(_jit, u, v);
}"""

for type in [""]:
	jit_putargr(type,type,"jit_gpr_t")
for type in ["_f","_d"]:
	jit_putargr(type,type,"jit_fpr_t")


print """
void fn_jit_putargi(jit_state_t *_jit, jit_word_t u, jit_node_t *v) {
	_jit_putargi(_jit, u, v);
}
void fn_jit_putargi_f(jit_state_t *_jit, jit_float32_t u, jit_node_t *v) {
	_jit_putargi_d(_jit, u, v);
}
void fn_jit_putargi_d(jit_state_t *_jit, jit_float64_t u, jit_node_t *v) {
	_jit_putargi_d(_jit, u, v);
}"""



print """
void fn_jit_prepare(jit_state_t *_jit) {
	_jit_prepare(_jit);
}
void fn_jit_ellipsis(jit_state_t *_jit) {	
	_jit_ellipsis(_jit);
}
void fn_jit_pushargr(jit_state_t *_jit, jit_gpr_t u) {
	_jit_pushargr(_jit,u);
}
void fn_jit_pushargi(jit_state_t *_jit, jit_word_t u) {
	_jit_pushargi(_jit, u);
}
void fn_jit_finishr(jit_state_t *_jit, jit_gpr_t r0) {
	_jit_finishr(_jit, r0);
}
jit_node_t *fn_jit_finishi(jit_state_t *_jit, jit_pointer_t i0) {
	return _jit_finishi(_jit, i0);
}
void fn_jit_ret(jit_state_t *_jit) {
	_jit_ret(_jit);
}
void fn_jit_retr(jit_state_t *_jit, jit_gpr_t u) {
	_jit_retr(_jit, u);
}
void fn_jit_reti(jit_state_t *_jit, jit_word_t u) {
	_jit_reti(_jit, u);
}"""


def jit_retval(name,type,r0_type):
	print """
void fn_jit_retval"""+name+"""(jit_state_t *_jit, """+r0_type+""" r0) {
	_jit_retval"""+type+"""(_jit, r0);
}"""

for type in ["_c","_uc","_s","_us","_i"]:
	jit_retval(type,type,"jit_gpr_t")
for type in ["_f","_d"]:
	jit_retval(type,type,"jit_fpr_t")
print """
#if __WORDSIZE == 32"""
jit_retval("","_i","jit_gpr_t")
print """
#else"""
for type in ["_ui","_l"]:
	jit_retval(type,type,"jit_gpr_t")
jit_retval("","_l","jit_gpr_t")
print """
#endif"""


print """
void fn_jit_epilog(jit_state_t *_jit) {
	_jit_epilog(_jit);
}

void fn_jit_patch(jit_state_t *_jit, jit_node_t *u) {
	_jit_patch(_jit,u);
}
void fn_jit_patch_at(jit_state_t *_jit, jit_node_t *u, jit_node_t *v) {
	_jit_patch_at(_jit,u,v);
}
void fn_jit_patch_abs(jit_state_t *_jit, jit_node_t *u, jit_pointer_t v) {
	_jit_patch_abs(_jit,u,v);
}
void fn_jit_realize(jit_state_t *_jit) {
	_jit_realize(_jit);
}
jit_pointer_t fn_jit_get_code(jit_state_t *_jit, jit_word_t *u) {
	return _jit_get_code(_jit,u);
}
void fn_jit_set_code(jit_state_t *_jit, jit_pointer_t u, jit_word_t v) {
	_jit_set_code(_jit,u,v);
}
jit_pointer_t fn_jit_get_data(jit_state_t *_jit, jit_word_t *u, jit_word_t *v) {
	return _jit_get_data(_jit,u,v);
}
void fn_jit_set_data(jit_state_t *_jit, jit_pointer_t u, jit_word_t v, jit_word_t w) {
	_jit_set_data(_jit,u,v,w);
}
void fn_jit_frame(jit_state_t *_jit, jit_int32_t u) {
	_jit_frame(_jit,u);
}
void fn_jit_tramp(jit_state_t *_jit, jit_int32_t u) {
	_jit_tramp(_jit,u);
}
jit_pointer_t fn_jit_emit(jit_state_t *_jit) {
	return _jit_emit(_jit);
}

void fn_jit_print(jit_state_t *_jit) {	
	_jit_print(_jit);
}
"""

def jit_arg(type):
	print """
jit_node_t *fn_jit_arg"""+type+"""(jit_state_t *_jit) {
	return _jit_arg"""+type+"""(_jit);
}"""

for type in ["","_f","_d"]:
	jit_arg(type)


print """
void fn_jit_pushargr_f(jit_state_t *_jit, jit_fpr_t u) {
	_jit_pushargr_f(_jit, u);
}
void fn_jit_pushargi_f(jit_state_t *_jit, jit_float32_t u) {
	_jit_pushargi_f(_jit, u);
}
void fn_jit_retr_f(jit_state_t *_jit, jit_fpr_t u) {
	_jit_retr_f(_jit, u);
}
void fn_jit_reti_f(jit_state_t *_jit, jit_float32_t u) {
	_jit_reti_f(_jit, u);
}

void fn_jit_pushargr_d(jit_state_t *_jit, jit_fpr_t u) {
	_jit_pushargr_d(_jit, u);
}
void fn_jit_pushargi_d(jit_state_t *_jit, jit_float64_t u) {	
	_jit_pushargi_d(_jit, u);
}
void fn_jit_retr_d(jit_state_t *_jit, jit_fpr_t u) {
	_jit_retr_d(_jit, u);
}
void fn_jit_reti_d(jit_state_t *_jit, jit_float64_t u) {
	_jit_reti_d(_jit, u);
}
"""


def jit_new_node_w(name,code):
	print """
jit_node_t *fn_jit_"""+name+"""(jit_state_t *_jit, jit_word_t u) {
	return _jit_new_node_w(_jit, jit_code_"""+code+""",u);
}"""

#grep -E 'jit_new_node_w[ \t\n]*\(' lightning.h | grep define | sed -E 's/^.* jit_([^(]+)\(.*$/"\1",/g' | tr -d "\n"
for code in ["live","align","jmpr","callr"]:
	jit_new_node_w(code,code)



def jit_new_node_p(name,code):
	print """
jit_node_t *fn_jit_"""+name+"""(jit_state_t *_jit, jit_pointer_t u) {
	return _jit_new_node_p(_jit, jit_code_"""+code+""",u);
}"""

#grep -E 'jit_new_node_p[ \t\n]*\(' lightning.h | grep define | sed -E 's/^.* jit_([^(]+)\(.*$/"\1",/g' | tr -d "\n"
for code in ["jmpi","calli"]:
	jit_new_node_p(code,code)


def call_with_hash(function, h):
	h_names = sorted(h.keys())
	for name in h_names:
		function(name, h[name])

def jit_new_node_ww(name,code):
	print """
jit_node_t *fn_jit_"""+name+"""(jit_state_t *_jit, jit_word_t u, jit_word_t v) {
	return _jit_new_node_ww(_jit, jit_code_"""+code+""",u,v);
}"""
	
#grep -E 'jit_new_node_ww[ \t\n]*\(' lightning.h | grep define | sed -E 's/^.* jit_([^(]+)\(.*$/"\1",/g' | tr -d "\n"
for code in ["negr","comr","movr","movi","extr_c","extr_uc","extr_s","extr_us","htonr_us","htonr_ui","ldr_c","ldr_uc","ldr_s","ldr_us","ldr_i","str_c","str_s","str_i","negr_f","absr_f","sqrtr_f","truncr_f_i","extr_f","extr_d_f","movr_f","ldr_f","str_f","negr_d","absr_d","sqrtr_d","truncr_d_i","extr_d","extr_f_d","movr_d","ldr_d","str_d","movr_w_f","movr_w_d","movr_f_w","movr_d_w"]:
	jit_new_node_ww(code,code)
h = {"ntohr_us":"htonr_us","ntohr_ui":"htonr_ui"}
call_with_hash(jit_new_node_ww, h)
print """
#if __WORDSIZE == 32"""
h = {"htonr":"htonr_ui","ntohr":"htonr_ui","str":"str_i","truncr_f":"truncr_f_i","truncr_d":"truncr_d_i"}
call_with_hash(jit_new_node_ww, h)
print """
#else"""
for code in ["extr_i","extr_ui","ldr_ui","ldr_l","str_l","truncr_f_l","truncr_d_l"]:
	jit_new_node_ww(code,code)
h = {"htonr_ul":"htonr_ul","ntohr_ul":"htonr_ul","htonr":"htonr_ul","ntohr":"htonr_ul","str":"str_l","truncr_f":"truncr_f_l","truncr_d":"truncr_d_l"}
call_with_hash(jit_new_node_ww, h)
print """
#endif"""



def jit_new_node_wp(name,code):
	print """
jit_node_t *fn_jit_"""+name+"""(jit_state_t *_jit, jit_word_t u, jit_pointer_t v) {
	return _jit_new_node_wp(_jit, jit_code_"""+code+""", u, v);
}"""

#grep -E 'jit_new_node_wp[ \t\n]*\(' lightning.h | grep define | sed -E 's/^.* jit_([^(]+)\(.*$/"\1",/g' | tr -d "\n"
for code in ["ldi_c","ldi_uc","ldi_s","ldi_us","ldi_i","ldi_f","ldi_d"]:
	jit_new_node_wp(code,code)
print """
#if __WORDSIZE == 32"""
h = {"ldr":"ldr_i","ldi":"ldi_i"}
call_with_hash(jit_new_node_wp, h)
print """
#else"""
for code in ["ldi_ui","ldi_l"]:
	jit_new_node_wp(code,code)
h = {"ldr":"ldr_l","ldi":"ldi_l"}
call_with_hash(jit_new_node_wp, h)
print """
#endif"""



def jit_new_node_pw(name,code):
	print """
jit_node_t *fn_jit_"""+name+"""(jit_state_t *_jit, jit_pointer_t u, jit_word_t v) {
	return _jit_new_node_pw(_jit, jit_code_"""+code+""", u, v);
}"""

#grep -E 'jit_new_node_pw[ \t\n]*\(' lightning.h | grep define | sed -E 's/^.* jit_([^(]+)\(.*$/"\1",/g' | tr -d "\n"
for code in ["sti_c","sti_s","sti_i","sti_f","sti_d"]:
	jit_new_node_pw(code,code)
print """
#if __WORDSIZE == 32"""
h = {"sti":"sti_i"}
call_with_hash(jit_new_node_pw, h)
print """
#else"""
for code in ["sti_l"]:
	jit_new_node_pw(code,code)
h = {"sti":"sti_l"}
call_with_hash(jit_new_node_pw, h)
print """
#endif"""



def jit_new_node_wf(name,code):
	print """
jit_node_t *fn_jit_"""+name+"""(jit_state_t *_jit, jit_word_t u, jit_float32_t v) {
	return _jit_new_node_wf(_jit, jit_code_"""+code+""",u,v);
}"""

for code in ["movi_f","movi_f_w"]:
	jit_new_node_wf(code,code)



def jit_new_node_wd(name,code):
	print """
jit_node_t *fn_jit_"""+name+"""(jit_state_t *_jit, jit_word_t u, jit_float64_t v) {
	return _jit_new_node_wd(_jit, jit_code_"""+code+""",u,v);
}"""

for code in ["movi_d","movi_d_w"]:
	jit_new_node_wd(code,code)



def jit_new_node_www(name, code):
	print """
jit_node_t *fn_jit_"""+name+"""(jit_state_t *_jit, jit_word_t u, jit_word_t v, jit_word_t w) {
	return _jit_new_node_www(_jit, jit_code_"""+code+""", u, v, w);
}"""

#grep -E 'jit_new_node_www[ \t\n]*\(' lightning.h | grep define | sed -E 's/^.* jit_([^(]+)\(.*$/"\1",/g' | tr -d "\n"
for code in ["addr","addi","addcr","addci","addxr","addxi","subr","subi","subcr","subci","subxr","subxi","rsbi","mulr","muli","divr","divi","divr_u","divi_u","remr","remi","remr_u","remi_u","andr","andi","orr","ori","xorr","xori","lshr","lshi","rshr","rshi","rshr_u","rshi_u","ltr","lti","ltr_u","lti_u","ler","lei","ler_u","lei_u","eqr","eqi","ger","gei","ger_u","gei_u","gtr","gti","gtr_u","gti_u","ner","nei","ldxr_c","ldxi_c","ldxr_uc","ldxi_uc","ldxr_s","ldxi_s","ldxr_us","ldxi_us","ldxr_i","ldxi_i","stxr_c","stxi_c","stxr_s","stxi_s","stxr_i","stxi_i","addr_f","subr_f","mulr_f","divr_f","ltr_f","ler_f","eqr_f","ger_f","gtr_f","ner_f","unltr_f","unler_f","uneqr_f","unger_f","ungtr_f","ltgtr_f","ordr_f","unordr_f","ldxr_f","ldxi_f","stxr_f","stxi_f","addr_d","subr_d","mulr_d","divr_d","ltr_d","ler_d","eqr_d","ger_d","gtr_d","ner_d","unltr_d","unler_d","uneqr_d","unger_d","ungtr_d","ltgtr_d","ordr_d","unordr_d","ldxr_d","ldxi_d","stxr_d","stxi_d","movr_ww_d","movr_d_ww"]:
	jit_new_node_www(code, code)
h = {"rsbr":"subr","rsbr_f":"subr_f","rsbr_d":"subr_d"}
call_with_hash(jit_new_node_www, h)
print """
#if __WORDSIZE == 32"""
h = {"ldxr":"ldxr_i","ldxi":"ldxi_i"}
call_with_hash(jit_new_node_www, h)
print """
#else"""
for code in ["ldxr_ui","ldxi_ui","ldxr_l","ldxi_l","stxr_l","stxi_l"]:
	jit_new_node_www(code, code)
h = {"ldxr":"ldxr_l","ldxi":"ldxi_l","stxr":"stxr_l","stxi":"stxi_l"}
call_with_hash(jit_new_node_www, h)
print """
#endif"""



def jit_new_node_qww(name,code):
	print """
jit_node_t *fn_jit_"""+name+"""(jit_state_t *_jit, jit_int32_t l, jit_int32_t h, jit_word_t v, jit_word_t w) {
	return _jit_new_node_qww(_jit, jit_code_"""+code+""",l, h, v, w);
}"""

#grep -E 'jit_new_node_qww[ \t\n]*\(' lightning.h | grep define | sed -E 's/^.* jit_([^(]+)\(.*$/"\1",/g' | tr -d "\n"
for code in ["qmulr","qmuli","qmulr_u","qmuli_u","qdivr","qdivi","qdivr_u","qdivi_u"]:
	jit_new_node_qww(code,code)



def jit_new_node_wwf(name,code):
	print """
jit_node_t *fn_jit_"""+name+"""(jit_state_t *_jit, jit_word_t u, jit_word_t v, jit_float32_t w) {
	return _jit_new_node_wwf(_jit, jit_code_"""+code+""",u,v,w);
}"""

#grep -E 'jit_new_node_wwf[ \t\n]*\(' lightning.h | grep define | sed -E 's/^.* jit_([^(]+)\(.*$/"\1",/g' | tr -d "\n"
for code in ["addi_f","subi_f","rsbi_f","muli_f","divi_f","lti_f","lei_f","eqi_f","gei_f","gti_f","nei_f","unlti_f","unlei_f","uneqi_f","ungei_f","ungti_f","ltgti_f","ordi_f","unordi_f"]:
	jit_new_node_wwf(code,code)



def jit_new_node_wwd(name, code):
	print """
jit_node_t *fn_jit_"""+name+"""(jit_state_t *_jit, jit_word_t u, jit_word_t v, jit_float64_t w) {
	return _jit_new_node_wwd(_jit, jit_code_"""+code+""",u,v,w);
}"""

#grep -E 'jit_new_node_wwd[ \t\n]*\(' lightning.h | grep define | sed -E 's/^.* jit_([^(]+)\(.*$/"\1",/g' | tr -d "\n"
for code in ["addi_d","subi_d","rsbi_d","muli_d","divi_d","lti_d","lei_d","eqi_d","gei_d","gti_d","nei_d","unlti_d","unlei_d","uneqi_d","ungei_d","ungti_d","ltgti_d","ordi_d","unordi_d","movi_d_ww"]:
	jit_new_node_wwd(code,code)



def jit_new_node_pww(name,code):
	print """
jit_node_t *fn_jit_"""+name+"""(jit_state_t *_jit, jit_pointer_t u, jit_word_t v, jit_word_t w) {
	return _jit_new_node_pww(_jit, jit_code_"""+code+""",u,v,w);
}"""

#grep -E 'jit_new_node_pww[ \t\n]*\(' lightning.h | grep define | sed -E 's/^.* jit_([^(]+)\(.*$/"\1",/g' | tr -d "\n"
for code in ["bltr","blti","bltr_u","blti_u","bler","blei","bler_u","blei_u","beqr","beqi","bger","bgei","bger_u","bgei_u","bgtr","bgti","bgtr_u","bgti_u","bner","bnei","bmsr","bmsi","bmcr","bmci","boaddr","boaddi","boaddr_u","boaddi_u","bxaddr","bxaddi","bxaddr_u","bxaddi_u","bosubr","bosubi","bosubr_u","bosubi_u","bxsubr","bxsubi","bxsubr_u","bxsubi_u","bltr_f","bler_f","beqr_f","bger_f","bgtr_f","bner_f","bunltr_f","bunler_f","buneqr_f","bunger_f","bungtr_f","bltgtr_f","bordr_f","bunordr_f","bltr_d","bler_d","beqr_d","bger_d","bgtr_d","bner_d","bunltr_d","bunler_d","buneqr_d","bunger_d","bungtr_d","bltgtr_d","bordr_d","bunordr_d"]:
	jit_new_node_pww(code,code)



def jit_new_node_pwf(name,code):
	print """
jit_node_t *fn_jit_"""+name+"""(jit_state_t *_jit, jit_pointer_t u, jit_word_t v, jit_float32_t w) {
	return _jit_new_node_pwf(_jit, jit_code_"""+code+""",u,v,w);
}"""

#grep -E 'jit_new_node_pwf[ \t\n]*\(' lightning.h | grep define | sed -E 's/^.* jit_([^(]+)\(.*$/"\1",/g' | tr -d "\n"
for code in ["blti_f","blei_f","beqi_f","bgei_f","bgti_f","bnei_f","bunlti_f","bunlei_f","buneqi_f","bungei_f","bungti_f","bltgti_f","bordi_f","bunordi_f"]:
	jit_new_node_pwf(code,code)



def jit_new_node_pwd(name,code):
	print """
jit_node_t *fn_jit_"""+name+"""(jit_state_t *_jit, jit_pointer_t u, jit_word_t v, jit_float64_t w) {
	return _jit_new_node_pwd(_jit, jit_code_"""+code+""",u,v,w);
}"""

#grep -E 'jit_new_node_pwd[ \t\n]*\(' lightning.h | grep define | sed -E 's/^.* jit_([^(]+)\(.*$/"\1",/g' | tr -d "\n"
for code in ["blti_d","blei_d","beqi_d","bgei_d","bgti_d","bnei_d","bunlti_d","bunlei_d","buneqi_d","bungei_d","bungti_d","bltgti_d","bordi_d","bunordi_d"]:
	jit_new_node_pwd(code,code)



print """
jit_bool_t fn_jit_arg_register_p(jit_state_t *_jit, jit_node_t *u) {
	return _jit_arg_register_p(_jit, u);
}
jit_bool_t fn_jit_callee_save_p(jit_state_t *_jit, jit_int32_t u) {
	return _jit_callee_save_p(_jit, u);
}
jit_bool_t fn_jit_pointer_p(jit_state_t *_jit,jit_pointer_t u) {
	return _jit_pointer_p(_jit, u);
}

jit_bool_t fn_jit_get_note(jit_state_t *_jit,jit_pointer_t n,char **u,char **v,int *w) {
	return _jit_get_note(_jit, n, u, v, w);
}

void fn_jit_disassemble(jit_state_t *_jit) {	
	_jit_disassemble(_jit);
}

void fn_jit_set_memory_functions(jit_alloc_func_ptr alloc_func_ptr,jit_realloc_func_ptr realloc_func_ptr,jit_free_func_ptr free_func_ptr) {
	jit_set_memory_functions(alloc_func_ptr, realloc_func_ptr, free_func_ptr);
}
void fn_jit_get_memory_functions(jit_alloc_func_ptr *alloc_func_ptr,jit_realloc_func_ptr *realloc_func_ptr,jit_free_func_ptr *free_func_ptr) {
	jit_get_memory_functions(alloc_func_ptr, realloc_func_ptr, free_func_ptr);
}
"""
