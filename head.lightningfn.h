//./c2ffi --std=c99 -i /usr/include/ -i /usr/include/i386-linux-gnu/ -i /usr/include/linux/ -o lightningfn ../../lightningfn/lightningfn.h

// size_t seems not to be defined by "--std=c99".
// not needed anymore because we don't need c2ffi anymore:
//typedef unsigned int size_t;

//#include <unistd.h>
//#include <stdlib.h>
/*#if HAVE_STDINT_H
#  include <stdint.h>
#endif
#include <string.h>
*/
#if defined(__hpux) && defined(__hppa__)
#  include <machine/param.h>
#endif
#if defined(__alpha__) && defined(__osf__)
#  include <machine/endian.h>
#endif

#ifndef __WORDSIZE
#  if defined(WORDSIZE)				/* ppc darwin */
#    define __WORDSIZE		WORDSIZE
#  elif defined(__SIZEOF_POINTER__)		/* ppc aix */
#    define __WORDSIZE		(__SIZEOF_POINTER__ << 3)
#  elif defined(_ILP32)				/* hppa hp-ux */
#    define __WORDSIZE		32
#  elif defined(_LP64)				/* ia64 hp-ux (with cc +DD64) */
#    define __WORDSIZE		64
#  elif defined(_MIPS_SZPTR)			/* mips irix */
#    if _MIPS_SZPTR == 32
#      define __WORDSIZE	32
#    else
#      define __WORDSIZE	64
#    endif
#  else						/* From FreeBSD 9.1 stdint.h */
#    if defined(UINTPTR_MAX) && defined(UINT64_MAX) && \
	(UINTPTR_MAX == UINT64_MAX)
#      define __WORDSIZE	64
#    else
#      define __WORDSIZE	32
#    endif
#  endif
#endif
#ifndef __LITTLE_ENDIAN
#  if defined(LITTLE_ENDIAN)			/* ppc darwin */
#    define __LITTLE_ENDIAN	LITTLE_ENDIAN
#  elif defined(__ORDER_LITTLE_ENDIAN__)	/* ppc aix */
#    define __LITTLE_ENDIAN	__ORDER_LITTLE_ENDIAN__
#  else
#    define __LITTLE_ENDIAN	1234
#  endif
#endif
#ifndef __BIG_ENDIAN
#  if defined(BIG_ENDIAN)			/* ppc darwin */
#    define __BIG_ENDIAN	BIG_ENDIAN
#  elif defined(__ORDER_BIG_ENDIAN__)		/* ppc aix */
#    define __BIG_ENDIAN	__ORDER_BIG_ENDIAN__
#  else
#    define __BIG_ENDIAN	4321
#  endif
#endif
#ifndef __BYTE_ORDER
#  if defined(BYTE_ORDER)			/* ppc darwin */
#    define __BYTE_ORDER	BYTE_ORDER
#  elif defined(__BYTE_ORDER__)			/* ppc aix */
#    define __BYTE_ORDER	__BYTE_ORDER__
#  elif defined(_BIG_ENDIAN)			/* hppa hp-ux */
#    define __BYTE_ORDER	__BIG_ENDIAN
#  elif defined(__BIG_ENDIAN__)			/* ia64 hp-ux */
#    define __BYTE_ORDER	__BIG_ENDIAN
#  elif defined(__i386__)			/* 32 bit x86 solaris */
#    define __BYTE_ORDER	__LITTLE_ENDIAN
#  elif defined(__x86_64__)			/* 64 bit x86 solaris */
#    define __BYTE_ORDER	__LITTLE_ENDIAN
#  elif defined(__MIPSEB)			/* mips irix */
#    define __BYTE_ORDER	__BIG_ENDIAN
#  else
#    error cannot figure __BYTE_ORDER
#  endif
#endif

typedef signed char		jit_int8_t;
typedef unsigned char		jit_uint8_t;
typedef signed short		jit_int16_t;
typedef unsigned short		jit_uint16_t;
typedef signed int		jit_int32_t;
typedef unsigned int		jit_uint32_t;
#if __WORDSIZE == 32
typedef signed long long	jit_int64_t;
typedef unsigned long long	jit_uint64_t;
typedef jit_int32_t		jit_word_t;
typedef jit_uint32_t		jit_uword_t;
#else
typedef signed long		jit_int64_t;
typedef unsigned long		jit_uint64_t;
typedef jit_int64_t		jit_word_t;
typedef jit_uint64_t		jit_uword_t;
#endif
typedef float			jit_float32_t;
typedef double			jit_float64_t;
typedef void*			jit_pointer_t;
typedef jit_int32_t		jit_bool_t;
typedef jit_int32_t		jit_gpr_t;
typedef jit_int32_t		jit_fpr_t;

typedef struct jit_node		jit_node_t;
typedef struct jit_state	jit_state_t;

typedef void* (*jit_alloc_func_ptr)	(size_t);
typedef void* (*jit_realloc_func_ptr)	(void*, size_t);
typedef void  (*jit_free_func_ptr)	(void*);

/* Functions without the standard "fn_jit_" prefix */

void init_fn_jit(const char *progname);
void finish_fn_jit(void);

/* Functions with the standard "fn_jit_" prefix */

