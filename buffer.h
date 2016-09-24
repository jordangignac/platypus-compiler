/*
*	File-Name : buffer.c
*	Author : Jordan Gignac
*	Purpose : Declare buffer data structure, standard headers, constansts and function prototypes for buffer.c source file.
*/

/* header guard */
#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */
/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#define R_FAIL_1 -1         /* fail return value */
#define R_FAIL_2 -2         /* fail return value */
#define LOAD_FAIL -2       /* load fail error */
#define SET_R_FLAG_1 1       /* realloc flag set value */
#define SET_R_FLAG_0 0      /* realloc flag set value */
#define B_OP_MODE_F 0		/* buffer fixed operation mode value */
#define B_OP_MODE_A 1		/* buffer additive operation mode value */
#define B_OP_MODE_M -1		/* buffer multiplicative operation mode value */
#define BUFFER_MAX_SIZE SHRT_MAX	/* max size of buffer, corresponding to max value of datatype */
#define SUCCESS 1	/* constast for successful function completion */
#define B_TRUE 1
#define B_FALSE 0

/* user data type declarations */
typedef struct BufferDescriptor {
	char *cb_head;   /* pointer to the beginning of character array (character buffer) */
	short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
	short addc_offset;  /* the offset (in chars) to the add-character location */
	short getc_offset;  /* the offset (in chars) to the get-character location */
	short mark_offset; /* the offset (in chars) to the mark location */
	char  inc_factor; /* character array increment factor */
	char  r_flag;     /* character array reallocation flag */
	char  mode;       /* operational mode indicator*/
	int   eob;       /* end-of-buffer reached flag */
} Buffer, *pBuffer;
/*typedef Buffer *pBuffer;*/
Buffer * b_create(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer * const, char);
int b_reset(Buffer * const pBD);
void b_destroy(Buffer * const pBD);
int b_isfull(Buffer * const pBD);
short b_size(Buffer * const pBD);
short b_capacity(Buffer * const pBD);
char * b_setmark(Buffer * const pBD, short mark);
short b_mark(Buffer * const pBD);
int b_mode(Buffer * const pBD);
size_t b_inc_factor(Buffer * const pBD);
int b_load(FILE * const fi, Buffer * const pBD);
int b_isempty(Buffer * const pBD);
int b_eob(Buffer * const pBD);
char b_getc(Buffer * const pBD);
int b_print(Buffer * const pBD);
Buffer * b_pack(Buffer * const pBD);
char b_rflag(Buffer * const pBD);
short b_retract(Buffer * const pBD);
short b_retract_to_mark(Buffer * const pBD);
short b_getc_offset(Buffer * const pBD);
#endif
