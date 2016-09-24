/*
*	File-Name : buffer.c
*	Author : Jordan Gignac
*	Purpose : Stores all constant definitions, function declarations and declarations which are required for the implementation of the symbol table
*/

/* header guard */
#ifndef STABLE_H_
#define STABLE_H_

/* includes */
#include "buffer.h"

/* Bit Mask defines */
#define MASK_ZERO 0x0000			/* 0000 0000 0000 0000 */
#define MASK_DEFAULT 0xFFF8			/* 1111 1111 1111 1000 */
#define MASK_RESET_UPDATE 0xFFFE	/* 1111 1111 1111 1110 */
#define MASK_RESET_DTI 0xFFF9		/* 1111 1111 1111 1001 */
#define MASK_SET_UPDATE 0x0001		/* 0000 0000 0000 0001 */
#define MASK_SET_INT 0x0004			/* 0000 0000 0000 0100 */				
#define MASK_SET_FPL 0x0002			/* 0000 0000 0000 0010 */
#define	MASK_SET_STR 0x0006			/* 0000 0000 0000 0110 */

typedef union InitialValue {
	int int_val;
	float fpl_val;
	int str_offset;
} InitialValue;

typedef struct SymbolTableVidRecord {
	unsigned short status_field;
	char *plex;
	int o_line;
	InitialValue i_value;
	size_t reserved;
} STVR;

typedef struct SymbolTableDescriptor {
	STVR *pstvr;
	int st_size;
	int st_offset;
	Buffer *plsBD;
} STD;

/* function prototypes */
STD st_create(int);
int st_install(STD, char *, char, int);
int st_lookup(STD, char *);
int st_update_type(STD, int, char);
int st_update_value(STD, int, InitialValue);
char st_get_type(STD, int);
void st_destroy(STD);
int st_print(STD);
int st_store(STD);
int st_sort(STD, char);

#endif
