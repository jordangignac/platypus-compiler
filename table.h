/*
*	File-Name : buffer.c
*	Author : Jordan Gignac
*	Purpose : Header file defining constants and variables
*/

/* header guards */
#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h>		/* NULL pointer constant is defined there */
#endif

#define SEOF 255			/* source end-of-file sentient symbol */
#define ES  12				/* Error state */
#define IS -1				/* Inavalid state */
#define TABLE_COLUMNS 7		/* State transition table definition */

/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	{ 1, 6, 4, 4, IS, IS, IS },		/* State 0 */
	{ 1, 1, 1, 1, 2, 3, 2 },			/* State 1 */
	{ IS, IS, IS, IS, IS, IS, IS },	/* State 2 */
	{ IS, IS, IS, IS, IS, IS, IS },	/* State 3 */
	{ ES, 4, 4, 4, 7, 5, 5 },			/* State 4 */
	{ IS, IS, IS, IS, IS, IS, IS },	/* State 5 */
	{ ES, ES, 9, ES, 7, ES, 5 },		/* State 6 */
	{ 8, 7, 7, 7, 8, 8, 8 },			/* State 7 */
	{ IS, IS, IS, IS, IS, IS, IS },	/* State 8 */
	{ ES, 9, 9, ES, ES, ES, 10 },	/* State 9 */
	{ IS, IS, IS, IS, IS, IS, IS },	/* State 10 */
	{ ES, ES, ES, ES, ES, ES, ES },	/* State 11 */
	{ IS, IS, IS, IS, IS, IS, IS },	/* State 12 */
	{ IS, IS, IS, IS, IS, IS, IS } };	/* State 13 */

/* Accepting state table definition */
#define ASWR     2  /* accepting state with retract */
#define ASNR     3  /* accepting state with no retract */
#define NOAS     1  /* not accepting state */

/* state table */
int as_table[] = { NOAS, NOAS, ASWR, ASNR, NOAS, ASWR, NOAS, NOAS, ASWR, NOAS, ASWR, NOAS, ASNR, ASWR };

/* Accepting action function declarations */
Token aa_func02(char *lexeme);
Token aa_func03(char *lexeme);
Token aa_func05(char *lexeme);
Token aa_func08(char *lexeme);
Token aa_func10(char *lexeme);
Token aa_func12(char *lexeme);

typedef Token(*PTR_AAF)(char *lexeme);	//defining a new type: pointer to function (of one char * argument) return token

/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
* Token (*aa_table[])(char lexeme[]) = {
*/
PTR_AAF aa_table[] = { NULL, NULL, aa_func02, aa_func03, NULL, aa_func05, NULL, NULL, aa_func08, NULL, aa_func10, NULL, aa_func12, aa_func12 };

/* Keyword lookup table (.AND. and .OR. are not keywords) */
#define KWT_SIZE  8

/* keyword table */
char * kw_table[] = {
	"ELSE",
	"IF",
	"INPUT",
	"OUTPUT",
	"PLATYPUS",
	"REPEAT",
	"THEN",
	"USING"
};
#endif
