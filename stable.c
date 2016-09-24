/*
*	File-Name : buffer.c
*	Author : Jordan Gignac
*	Purpose : Parse an input stream of characters from a buffer and group into tokens based on lexical and grammatical analysis of the characters
*/
#define _CRT_SECURE_NO_WARNINGS

/* includes */
#include <stdlib.h>
#include <string.h>
#include "stable.h"

/* global sym_table */
extern STD sym_table;

/* function prototypes for 'internal' functions */
static void st_setsize(void);
static void st_incoffset(void);

/*
*	Purpose : Function creates a new symbol table using the inputted size
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : Receives an integer corresponding to table size
*	Return Value : Returns a symbol table object upon creation
*/
STD st_create(int st_size) {
	STD std;	//create the symbol table object
	std.st_offset = 0;	//sets symbol table offset to default 0
	std.st_size = 0;	//sets symbol size to default 0

	std.pstvr = (STVR *)malloc(st_size * sizeof(STVR));	//allocates enough memory for stvr array with size 
	std.plsBD = b_create(1, 1, 'a');	//creates self incrementing buffer with space for 1 item

	std.st_size = st_size;	//if successful sets symbol table size to inputted size
	return std;	//returns newly created symbol table
}

/*
*	Purpose : Function attempts to install vid into symbol table
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : Receives a symbol table object, pointer to char array for lexeme, char for vid type, and line corresponding to line number
*	Return Value : Returns an integer corresponding to offset of vid in table
*/
int st_install(STD sym_table, char *lexeme, char type, int line) {
	int offset, pOffset = 0;	//variable used to store offset
	unsigned int i;	//variable for interation

	if (sym_table.st_size == 0) { return -1; }	//make sure symbol table is not empty else return -1

	offset = st_lookup(sym_table, lexeme);	//lookup to see if entry already exists in table

	if (offset != -1){ return offset; }	//if entry already exists in table then offset to entry is returned

	offset = sym_table.st_offset;	//set offset temp variable to table offset

	if (offset >= sym_table.st_size) { return -1; }	//if offset is equal or greater than table size then return error

	sym_table.pstvr[offset].plex = b_setmark(sym_table.plsBD, b_size(sym_table.plsBD));	//set buffer to start of lexeme for plex
	sym_table.pstvr[offset].o_line = line;	//sets line occurrence number to the supplied line

	/* loop from start of lexeme to end and add to buffer */
	for (i = 0; i<strlen(lexeme); i++){
		b_addc(sym_table.plsBD, lexeme[i]);
	}
	b_addc(sym_table.plsBD, '\0');	//add string termination character to the end to properly format c-string

	sym_table.pstvr[offset].status_field = MASK_ZERO;	//sets status field to zero
	sym_table.pstvr[offset].status_field |= MASK_DEFAULT;	//bitwise | with default mask to set status field to default bit values

	/* switch to determine data type and set indicator in stvr */
	switch (type) {
	case 'I':
		sym_table.pstvr[offset].status_field |= MASK_SET_INT;	//sets status field to integer using bitwise |
		sym_table.pstvr[offset].i_value.int_val = 0;	//sets integer value to default value
		break;
	case 'F':
		sym_table.pstvr[offset].status_field |= MASK_SET_FPL;	//sets status field to floting point literal using bitwise |
		sym_table.pstvr[offset].i_value.fpl_val = 0.0f;	//sets floating point literal value to default value
		break;
	case 'S':
		sym_table.pstvr[offset].status_field |= MASK_SET_STR;	//sets status field to string literal using bitwise |
		sym_table.pstvr[offset].status_field |= MASK_SET_UPDATE;	//sets the update bit in the status field
		sym_table.pstvr[offset].i_value.str_offset = -1;	//sets string offset value to default value
		break;
	}

	/* loops through symbol table and calculates offset to new lexeme */
	for (i = 0; i <= sym_table.st_offset; i++){
		sym_table.pstvr[i].plex = b_setmark(sym_table.plsBD, pOffset);
		pOffset += strlen(sym_table.pstvr[i].plex) + 1;
	}

	st_incoffset();	//increments the symbol table offset because a new item has been added
	return offset;	//returns offset to lexeme
}

/*
*	Purpose : Function searches through symbol table for a specific lexeme
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : Receives a symbol table object and pointer to a char array corresponding to the lexeme
*	Return Value : Returns offset to the vid or -1 if it cannot be found
*/
int st_lookup(STD sym_table, char *lexeme){
	int i;	//variable used for iteration
	int offset = -1;

	/* loops trhough symbol table to compare lexeme with symbol table plex */
	for (i = sym_table.st_offset - 1; i >= 0; i--){		//decrements to determine offset from end of table
		if (strcmp(sym_table.pstvr[i].plex, lexeme) == 0)	//if lexeme is equal to plex in table at index
			offset = sym_table.st_offset;  /*returns the offset of the entry from the beginning of the array of STVR*/
	}
	return offset;	//returns offset, if it hasnt been found it returns -1
}

/*
*	Purpose : Function updates the data type indicator for the specific vid
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : Receives a symbol table object, integer corresponding to vid offset in table, and char corresponding to vid type
*	Return Value : Returns an int corresponding to vid offset in table
*/
int st_update_type(STD sym_table, int vid_offset, char v_type) {
	unsigned short tempMask;	//temp mask used to determine update type
	if (sym_table.st_size == 0) { return -1; }	//make sure symbol table is not empty else return -1

	tempMask = sym_table.pstvr[vid_offset].status_field & MASK_SET_UPDATE;	//bitwise & with update mask to determine status field update status

	if (tempMask) { return -1; }	//if update flag is already set then return -1

	sym_table.pstvr[vid_offset].status_field &= MASK_RESET_DTI;	//reset status field bits

	/* switch to determine vid type from v_type */
	switch (v_type){
	case 'I':
		sym_table.pstvr[vid_offset].status_field |= MASK_SET_INT;	//bitwise | to set status field DTI to integer value
		break;
	case 'F':
		sym_table.pstvr[vid_offset].status_field |= MASK_SET_FPL;	//bitwise | to set status field DTI to floating point literal value
		break;
	default:
		return -1;	//returns -1 if the v_type is not valid	
	}

	sym_table.pstvr[vid_offset].status_field |= MASK_SET_UPDATE;	//set flag to updated before returning

	return vid_offset;	//returns vid_offset upon successful completion
}

/*
*	Purpose : Function updates vid at offset with initial value
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : Receives a symbol table object, int corresponding to vid offset in table, and an initial value object
*	Return Value : Returns an integer corresponding to vid offset in table
*/
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value) {
	if (sym_table.st_size == 0) { return -1; }	//make sure symbol table is not empty else return -1

	sym_table.pstvr[vid_offset].i_value = i_value;	//sets initial value of STVR based on offset

	return vid_offset;	//returns vid_offset upon successful completion
}

/*
*	Purpose : Function returns the type of the variable specified by vid_offset in the table
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : Receives a symbol table object, and integer corresponding to vid offset in table
*	Return Value : Returns a char corresponding to the variables type
*/
char st_get_type(STD sym_table, int vid_offset) {
	unsigned short tempMask;	//temp mask used to determine type of vid

	if (sym_table.st_size == 0) { return -1; }	//make sure symbol table is not empty else return -1

	tempMask = sym_table.pstvr[vid_offset].status_field & MASK_SET_STR;	//bitwise & sets status field to a format similar to MASK_SET_..

	/* checks tempMask against integer and float masks and returns appropriate char, else returns char for string VID */
	if (tempMask == MASK_SET_INT){
		return 'I';	//return i char for integer type
	}
	else if (tempMask == MASK_SET_FPL){
		return 'F';	//returns f char for floating point literal type
	}
	else {
		return 'S';	//returns s char for string type if any other value
	}
}

/*
*	Purpose : Function frees memory occupied by symobl table dynamic areas
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : Receives a symbol table object
*	Return Value : n/a
*/
void st_destroy(STD sym_table) {
	if (sym_table.plsBD) { b_destroy(sym_table.plsBD); }	//destory buffer if it exists
	if (sym_table.pstvr) { free(sym_table.pstvr); }	//free symbol table using pointer if exists
	st_setsize();	//set symbol table size to 0
}

/*
*	Purpose : Function prints the contents of the symbol table to the standard output
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : Receives a symbol table object
*	Return Value : Returns an integer corresponding to the number of entries printed
*/
int st_print(STD sym_table) {
	int i;	//iterator variable used for offset calculations
	if (sym_table.st_size == 0) { return -1; }	//make sure symbol table is not empty

	printf("\nSymbol Table\n__________________\n\nLine Number\tVariable Identifier\n");	//print titles for display

	//printf("%d", sym_table.st_offset);
	for (i = 0; i<sym_table.st_offset; i++){
		printf("%2d\t\t\t%s\n", sym_table.pstvr[i].o_line, sym_table.pstvr[i].plex);	//prints line number occurrence and lexeme
	}

	return sym_table.st_offset;	//returns the number of entry printed
}

/*
*	Purpose : Function stores the symbol table into a file named $stable.ste
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : Receives a symbol table object
*	Return Value : Returns an integer corresponding to the number of symbols in the table
*/
int st_store(STD sym_table) {
	FILE *tempFile;	//Pointer to file for ouput
	int i = 0;	//iterator variable, declared here so it can be returned

	if (sym_table.st_size == 0) { return -1; }	//make sure symbol table is not empty else return -1

	tempFile = fopen("$stable.ste", "w");	//open file output stream with overwriting mode

	fprintf(tempFile, "%d ", sym_table.st_size);	//writes symbol table size to file first

	for (i = 0; i<sym_table.st_offset; i++){
		fprintf(tempFile, "%4X ", sym_table.pstvr[i].status_field);	//writes status field in hex to file
		fprintf(tempFile, "%d ", strlen(sym_table.pstvr[i].plex));	//writes lexeme seize to file
		fprintf(tempFile, "%s ", sym_table.pstvr[i].plex);	//writes lexeme to file
		fprintf(tempFile, "%d ", sym_table.pstvr[i].o_line);

		/* determine symbol table type so initial value can be printed */
		switch (st_get_type(sym_table, i)){
		case 'I':
			fprintf(tempFile, "%d ", sym_table.pstvr[i].i_value.int_val);	//if type is int print integer value attribute
			break;
		case 'F':
			fprintf(tempFile, "%.2f ", sym_table.pstvr[i].i_value.fpl_val);	//if type is float print floating point value attribute
			break;
		case 'S':
			fprintf(tempFile, "%d ", sym_table.pstvr[i].i_value.str_offset);	//if type is string print string offset value attribute
			break;
		}
	}

	fclose(tempFile);	//close the output stream
	printf("\nSymbol Table stored.\n");	//print out if symtable has been successfully written
	return i;	//return number of symbols
}

/*
*	Purpose : Function sets symbol table offset to 0
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : n/a
*	Return Value : n/a
*/
static void st_setsize(void) {
	sym_table.st_size = 0;	//sets st_size to 0 for functions that don't have access
}

/*
*	Purpose : Function increments symbol table offset
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : n/a
*	Return Value : n/a
*/
static void st_incoffset(void) {
	sym_table.st_offset++;	//increments st_offset for functions that don't have access
}

/*
*	Purpose : Function sorts symbol table
*	Author : n/a
*	Version : n/a
*	Parameters : n/a
*	Return Value : n/a
*/
int st_sort(STD sym_table, char s_order) {
	return 0;	//return 0 as function is not used currently
}