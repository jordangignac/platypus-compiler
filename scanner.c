/*
*	File-Name : buffer.c
*	Author : Jordan Gignac
*	Purpose : Parse an input stream of characters from a buffer and group into tokens based on lexical and grammatical analysis of the characters
*/
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

//#define NDEBUG        to suppress assert() call
#include <assert.h>	 /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"
#include "stable.h"

#define DEBUG	/* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals. It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/* pointer to temporary lexeme buffer */
extern STD sym_table;	/* global sym_table variable */

/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */
static long atool(char * lexeme); /* converts octal string to decimal value */

int scanner_init(Buffer * sc_buf) {
	if (b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_setmark(sc_buf, 0);
	b_retract_to_mark(sc_buf);
	b_reset(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;
}

/*
*	Purpose : Function reads in characters from buffer and returns the next token that has been captured from the input stream
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : b_getc(), b_retract(), b_retract_to_mark(), b_setmark(), b_getc_offset(), b_size(), b_addc(), isalnum(), b_destroy()
*	Parameters : Receives a pointer to a buffer structure
*	Return Value : Returns a token corresponding to the last n characters of the scanner
*	Algorithm : Uses a while loop to feed through incoming characters and a switch case to determine what token needs to be returned.  Utilizes buffer retract and mark
*				functions to allow scanner to step through and determine lexically correct lexemes
*/
Token mlwpar_next_token(Buffer *sc_buf)
{
	Token t; /* token to return after recognition */
	unsigned char c; /* input symbol */
	int isError = 0;		/* boolean style int used for error checking */
	int i = 0;	/* iterator */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input buffer */
	short lexend;    /*end   offset of a lexeme in the input buffer */
	int accept = NOAS; /* type of state - initially not accepting */

	/* endless loop broken by token returns it will generate a warning */
	while (1) {
		c = b_getc(sc_buf);		/* get next character */
		switch (c){
		case '\0':
		case SEOF:	/* end of file def, returns SEOF token */
			t.code = SEOF_T;
			return t;
			break;
		case '\t':	/* horizontal tab case */
			continue;
		case 'v':	/* vertical tab case */
			continue;
		case ' ':	/* white space case */
			continue;
		case '\n':	/* new line case, increments line number */
			line++;
			break;
		case '!':	/* '!' case, checks for following '<' character to signal comment, else returns error */
			c = b_getc(sc_buf);
			if (c == '<'){
				while (c != '\n'){	/* read until new line character */
					c = b_getc(sc_buf);
				}
				line++;
			}
			else {		/* retracts buffer back to original position and returns error token with '!' and one error character */
				b_retract(sc_buf);
				b_retract(sc_buf);
				c = b_getc(sc_buf);
				t.code = ERR_T;
				t.attribute.err_lex[0] = c;
				c = b_getc(sc_buf);
				t.attribute.err_lex[1] = c;
				t.attribute.err_lex[2] = '\0';

				/* loop through remaining characters until new line is reached to continue parsing */
				while (c != '\n'){
					c = b_getc(sc_buf);
				}
				return t;
			}
			break;
		case '<':	/* '<' case checks for following '>' character to signal relational operation not equal, else returns less than relational operation token */
			c = b_getc(sc_buf);
			if (c == '>'){
				t.code = REL_OP_T;	/* set token code */
				t.attribute.rel_op = NE;	/*set token attribute using rel_op enum */
				return t;
			}
			else {
				b_retract(sc_buf);	/* retract buffer if next character is not '>' to original position */
				t.code = REL_OP_T;
				t.attribute.rel_op = LT;
				return t;
			}
			break;
		case '>':	/* '>' case, sets token to greater than relational operation and returns token */
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;
			break;
		case '#':	/* '#' case, sets token to string concatenation code and returns token */
			t.code = SCC_OP_T;
			return t;
			break;
		case '.':
			/* '.' case, checks if the following characters match the logical operators syntax
			*	AND. for AND logical operator or OR. for OR logical operator, else returns error token
			*/
			b_setmark(sc_buf, b_getc_offset(sc_buf) - 1);		/* sets the buffer mark just before the '.' character in case it needs to be recalled because of error */
			c = b_getc(sc_buf);
			isError = 0;	/* sets isError to false, used to set error token and save space */
			if (c == 'A'){
				c = b_getc(sc_buf);
				if (c == 'N'){
					c = b_getc(sc_buf);
					if (c == 'D'){
						c = b_getc(sc_buf);
						if (c == '.'){
							t.code = LOG_OP_T;
							t.attribute.log_op = AND;
							return t;
							break;
						}
						else { isError = 1; }	/* sets isError to true if the characters do not match syntax */
					}
					else { isError = 1; }	/* sets isError to true if the characters do not match syntax */
				}
				else { isError = 1; }	/* sets isError to true if the characters do not match syntax */
			}
			else if (c == 'O'){
				c = b_getc(sc_buf);
				if (c == 'R'){
					c = b_getc(sc_buf);
					if (c == '.'){
						t.code = LOG_OP_T;
						t.attribute.log_op = OR;
						return t;
						break;
					}
					else { isError = 1; }	/* sets isError to true if the characters do not match syntax */
				}
				else { isError = 1; }	/* sets isError to true if the characters do not match syntax */
			}
			else { isError = 1; }	/* sets isError to true if the characters do not match syntax */

			/* if isError retracts buffer before '.' character and stores/returns it into error token */
			if (isError == 1) {
				b_retract_to_mark(sc_buf);	/* retract to starting position */
				c = b_getc(sc_buf);	/* get next char */
				t.code = ERR_T;	/* set error code */
				t.attribute.err_lex[0] = c;	/* sets err_lex to error character */
				t.attribute.err_lex[1] = '\0';	/* terminating character for string */
				return t;	/* return token */
				break;
			}
			break;
		case '{':	/* '{' case which return token corresponding to left bracket */
			t.code = LBR_T;
			return t;
			break;
		case '}':	/* '}' case which return token corresponding to right bracket */
			t.code = RBR_T;
			return t;
			break;
		case '(':	/* '(' case which return token corresponding to left parenthesis */
			t.code = LPR_T;
			return t;
			break;
		case ')':	/* ')' case which return token corresponding to right parenthesis */
			t.code = RPR_T;
			return t;
			break;
		case ',':	/* ',' case which return token corresponding to comma */
			t.code = COM_T;
			return t;
			break;
		case '=':
			/* '=' case, checks if next character is also '=', if so returns token corresponding to equate relational operator
			*	if not, retracts buffer and return token corresponding to the assignment operator
			*/
			c = b_getc(sc_buf);
			if (c == '='){
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
				return t;
				break;
			}
			else {
				b_retract(sc_buf);	/* retract buffer back to original position */
			}
			t.code = ASS_OP_T;
			return t;
			break;
		case '+':	/* '+' case, returns token corresponding to PLUS arithmetic operator */
			t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;
			break;
		case '-':	/* '-' case, returns token corresponding to MINUS arithmetic operator */
			t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;
			break;
		case '*':	/* '*' case, returns token corresponding to MULT arithmetic operator */
			t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;
			break;
		case '/':	/* '/' case, returns token corresponding to DIV arithmetic operator */
			t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;
			break;
		case ';':	/* ';' case, returns token corresponding to end of statement */
			t.code = EOS_T;
			return t;
			break;
		case '"':
			/*
			*/
			b_setmark(sc_buf, b_getc_offset(sc_buf));	/* set mark of buffer at start of 'string' */
			lexstart = b_getc_offset(sc_buf);	/* sets the start of the lexeme at the mark */
			c = b_getc(sc_buf);
			while (c != '"'){	/* loops through characters while the end of the 'string' has not been reached */
				if (c == '\n'){ line++; }	/* if a new line is reached continue on and increment line number */
				if (c == SEOF || c == '\0'){		/* if scanner end of file is reached, return error token */
					t.code = ERR_T;
					lexend = b_getc_offset(sc_buf);	/*set lexeme end to the end of file character */
					b_setmark(sc_buf, lexstart - 1);
					b_retract_to_mark(sc_buf);	/* retract buffer back to orgin */
					//b_retract(sc_buf);	/* retract buffer once more to include '"' in error token error lex */
					//lexstart--;	/* decrement lexstart to match the retracted buffer */

					i = 0;	/* make sure iterator is set to 0 */
					while (lexstart != lexend){		/* loops through characters until seof/lexend is reached */
						c = b_getc(sc_buf);	/* get next character */
						if (i<ERR_LEN){		/* if number of characters is still less than err_lex size */
							t.attribute.err_lex[i++] = c;	/* add character to err_lex and increment iterator */
						}
						lexstart++;	/* increment lexstart ahead in the buffer */
					}
					/* adds ellipses to error output to show it is longer than allowed err_lex size; to match teacher output */
					t.attribute.err_lex[ERR_LEN - 3] = '.';
					t.attribute.err_lex[ERR_LEN - 2] = '.';
					t.attribute.err_lex[ERR_LEN - 1] = '.';
					t.attribute.err_lex[ERR_LEN] = '\0';
					return t;
				}

				c = b_getc(sc_buf);	/* get next character */
			}
			lexend = b_getc_offset(sc_buf) - 1;	/* decrement lexend to compensate for '"' */
			t.code = STR_T;	/* set token code */
			t.attribute.str_offset = b_size(str_LTBL);	/* set the str offset in the string literal buffer using the b_size function */
			b_retract_to_mark(sc_buf);	/* retracts buffer back to mark to begin reading string */
			while (lexstart != lexend) {	/* reads in characters until end of lexeme has been reached */
				c = b_getc(sc_buf);
				b_addc(str_LTBL, c);
				++lexstart;
			}
			b_addc(str_LTBL, '\0');	/* uses buffer function to add string termination character to string literal buffer */
			c = b_getc(sc_buf);	/* gets next character before breaking to get rid of trailing '"' character */
			return t;
			break;
		default:
			if (isalnum(c)){	/* checks if character is alphabet or numeric character */
				b_setmark(sc_buf, b_getc_offset(sc_buf) - 1);	/* sets mark of buffer to right before current character */
				lexstart = b_getc_offset(sc_buf) - 1;	/* sets lexeme start to right before current character */
				state = 0;	/* sets state to 0 */
				b_retract(sc_buf);	/* retract buffer back one character */

				while (accept == NOAS){		/* while state is not accepting */
					c = b_getc(sc_buf);
					state = get_next_state(state, c, &accept);	/* use character to determine next state using the get_next_state function to navigate the state table */
				}

				if (accept == ASWR){		/* if the state requires a buffer retract peform that here */
					b_retract(sc_buf);
				}

				lexend = b_getc_offset(sc_buf);	/* sets lexeme end to current buffer position before retracting */
				lex_buf = b_create((lexend - lexstart) + 1, 1, 'f');	/* creates temporary buffer to hold lexeme */
				b_retract_to_mark(sc_buf);

				/* read lexeme into temporary buffer */
				while (lexstart != lexend){
					c = b_getc(sc_buf);
					b_addc(lex_buf, c);
					++lexstart;
				}

				b_addc(lex_buf, '\0');	/* add string termination character */
				t = aa_table[state](lex_buf->cb_head);	/* uses function table to call the correct function for the state, returning a token into t variable */
				b_destroy(lex_buf);	/* free memory associated with temporary buffer */
				return t;
			}
			else {	/* if character does not match isalnum() then return error token for character */
				t.code = ERR_T;	/* sets error code */
				t.attribute.err_lex[0] = c;	/* sets character to token attribute */
				t.attribute.err_lex[1] = '\0';	/* adds string termination character */
				return t; /* return token */
			}
			break;	/* break from default case*/
		}
	}
}

//DO NOT MODIFY THE CODE OF THIS FUNCTION
//YOU CAN REMOVE THE COMMENTS
int get_next_state(int state, char c, int *accept){
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif

	assert(next != IS);

#ifdef DEBUG
	if (next == IS){
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

/*
*	Purpose : Determines the character class of the char in the table and returns the column number
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : isAlpha(), isdigits()
*	Parameters : Receives a character
*	Return Value : Returns an integer value corresponding to the character classes column number
*	Algorithm : Checks if the character is alphabet, 0, digit between 1-7, digit between 8-9, '.', '%', or if the character is 'other'
*				and returns column number accordingly
*/
int char_class(char c){
	int col = -1;	/* defines col int for defining char class in table */

	if (isalpha(c)) {	/* if character is alphabet then return column 0 */
		col = 0;
	}
	else if (isdigit(c)) {	/* is character a digit */
		if (c == '0') {		/* if character is equal to 0 return column 1 */
			col = 1;
		}
		else if (c < '8') {		/* if character is digit and is less than 8 return column 2*/
			col = 2;
		}
		else if (c > '7') {		/* if character is a digit and is greater than 7 return column 3 */
			col = 3;
		}
	}
	else if (c == '.') {	/* if character is decimal return column 4 */
		col = 4;
	}
	else if (c == '%'){		/* if character is percent return column 5 */
		col = 5;
	}
	else { col = 6; };		/* if character is 'other' return column 6 */
	return col;		/* return column number to calling function */
}

/*
*	Purpose : Function is called if lexeme is determined to be arithmetic variable identifier or keyword, if valid, returns token accordingly
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : iskeyword(), strncpy(), strcpy()
*	Parameters : Receives a char array
*	Return Value : Returns a token corresponding to either an arithmetic variable identifier or keyword
*	Algorithm : Checks first if lexeme is part of the kwtable, and if so sets token values to keyword, if not, then lexeme is AVID and the char array is copied into the
*				corresponding attribute and code set, token is then returned to calling function
*/
Token aa_func02(char lexeme[]){
	Token t;
	int keyIndex = iskeyword(lexeme);	/* checks if lexeme is keyword and if so returns index in table or negative value */
	char *newLex = (char *)malloc(sizeof(char)* (size_t)VID_LEN + 1);		/* creates new character array to hold variable identifier */

	int vid_offset;		/* temp variable for storing vid_offset */

	if (keyIndex >= 0){		/* if lexeme is keyword set token to keyword and set keyindex value */
		t.code = KW_T;
		t.attribute.kwt_idx = keyIndex;
	}
	else{
		if (strlen(lexeme) > VID_LEN){		/* if lexeme length is greater than variable identifier length then copy only VID_LEN values and append string terminating character*/
			strncpy(newLex, lexeme, VID_LEN);
			newLex[VID_LEN] = '\0';
		}
		else {	/* if lex is smaller than VID_LEN simply copy string into newlex */
			strcpy(newLex, lexeme);
			newLex[strlen(lexeme)] = '\0';
		}

		/* if variable starts with i,o,d, or w variable is integer, else variable type is float */
		if (lexeme[0] == 'i' || lexeme[0] == 'o' || lexeme[0] == 'd' || lexeme[0] == 'w'){
			vid_offset = st_install(sym_table, newLex, 'I', line);	// calls the vid install function from stable.c to install newLex as integer
		}
		else {
			vid_offset = st_install(sym_table, newLex, 'F', line);	// calls the vid install function from stable.c to install newLex as floating point literal
		}

		t.code = AVID_T;	/* set token code to correct variable identifier */
		t.attribute.vid_offset = vid_offset;	//sets vid offset to value returned by previous functions
	}

	/*If the symbol table is full, prints error message and calls function to store symbol table, then exits */
	if (sym_table.st_offset == sym_table.st_size){	//if next symbol install location is equal to table size
		printf("\nError: The Symbol Table is full - install failed.\n");	//prints error
		st_store(sym_table);	//store table into file
		exit(0);	//exits program
	}

	free(newLex);

	return t;	//returns token if all is successfull
}

/*
*	Purpose : Function is called if lexeme is determined to be string variable identifier, returns token accordingly
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : strncpy(), strcpy()
*	Parameters : Receives a char array
*	Return Value : Returns a token corresponding to a string variable identifier
*	Algorithm : Creates temporary char array, copies lexeme into temp array up until variable identifier length, finally sets token code and copies temp array into token attribute and returns token
*/
Token aa_func03(char lexeme[]){
	Token t;
	int offset;
	char *temp = NULL;

	t.code = SVID_T;

	if (strlen(lexeme) >= VID_LEN){
		temp = (char *)malloc(sizeof(char)* VID_LEN + 1);
		strncpy(temp, lexeme, VID_LEN);
		temp[VID_LEN - 1] = '%';
		temp[VID_LEN] = '\0';
		lexeme = temp;
	}

	offset = st_install(sym_table, lexeme, 'S', line);

	if (temp) { free(temp); }

	if (offset == -1){
		printf("\nError: The Symbol Table is full - install failed.\n");	//prints error
		st_store(sym_table);	//store table into file
		exit(0);	//exits program
	}

	t.attribute.vid_offset = offset;

	return t;









	//if (strlen(lexeme)>=VID_LEN){	/* if lexeme is greater than VID_LEN, copy over solely VID_LEN characters of lexeme */
	//	strncpy(newLex, lexeme, VID_LEN);
	//	newLex[VID_LEN - 1] = '%';	/* set percent sign to end of string to symbolize that the variable is still a string, despite being concatenated slightly */
	//	newLex[VID_LEN] = '\0';	/* adds string termination character at the end of the string */
	//}
	//else {	/* if lexeme is smaller than VID_LEN copy over all characters of lexeme then append string termination character at the end of the arry */
	//	strncpy(newLex, lexeme, VID_LEN);
	//	newLex[strlen(lexeme)] = '\0';
	//}
	//t.code = SVID_T;	/* sets token code to string variable identifier */
	//t.attribute.vid_offset = st_install(sym_table, newLex, 'S', line);	//calls install function with string input parameters and newlex

	///*If the symbol table is full, prints error message and calls function to store symbol table, then exits */
	//if (sym_table.st_offset == sym_table.st_size){	//if next symbol install location is equal to table size
	//       printf("\nError: The Symbol Table is full - install failed.\n");	//prints error
	//       st_store(sym_table);	//store table into file
	//       exit(0);	//exits program
	//   }
	//return t;
}

/*
*	Purpose : Function is called if lexeme is determined to be an integer literal value, returns token accordingly
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : atoi(), strncpy()
*	Parameters : Receives a char array
*	Return Value : Returns a token corresponding to integer literal or error token
*	Algorithm : Converts lexeme c-string to integer value, if integer value is valid and within bounds of 2-byte integer then the token code is set
*				and the integer literal value is stored into the token attribute, the token is returned to calling function
*/
Token aa_func05(char lexeme[]){
	Token t;
	int val = atoi(lexeme);	/* converts c-string to integer value */

	if (val > SHRT_MAX || val < 0){	/* if value is greater than int_max  or less than zero return error token */
		t.code = ERR_T;
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN);	/* set attribute to integer that caused issue up to ERR_LEN */
		t.attribute.err_lex[ERR_LEN] = '\0';	/* append string termination character */
	}
	else {
		t.code = INL_T;	/* if valid integer set token code to integer literal */
		t.attribute.int_value = val;	/* set integer value of token to val */
	}
	return t;	/* return token */
}

/*
*	Purpose : Function is called if lexeme is determined to be a floating point literal, returns token accordingly
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : strncpy(), atof()
*	Parameters : Receives a char array
*	Return Value : Returns a token corresponding to floating point literal or error token
*	Algorithm : Converts lexeme into double value using atof, if val is not within valid float ranges then an error token is returned, if it is valid casts double value
*				into float value of token and returns token
*/
Token aa_func08(char lexeme[]){
	Token t;
	double val = atof(lexeme);	/* converts lexeme to floating point number, returns double, cast to float later */

	if (val > FLT_MAX || (val < FLT_MIN && val > 0.0)) { /* if number is greater than float max or less than float min while still above 0 returns error token */
		t.code = ERR_T;
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN);
		t.attribute.err_lex[ERR_LEN] = '\0';
	}
	else {
		t.code = FPL_T;	/* sets token to floating point literal */
		t.attribute.flt_value = val;	/* sets double value to float value in token, typecasting to float type */
	}
	return t;	/* return token */
}

/*
*	Purpose : Function is called if lexeme corresponds to octal integer literal value, returns token
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : strncpy(), atool()
*	Parameters : Receives a char array
*	Return Value : Returns a token corresponding to octal integer literal
*	Algorithm : Uses atool function to convert c-string into octal integer, if val is not within 2-byte integer values then an error token is returned,
*				if not the value is stored into the token attribute and the token is returned
*/
Token aa_func10(char lexeme[]){
	Token t;
	int val = atool(lexeme);	/* uses atool to convert c-string to octal integer */

	if (val > SHRT_MAX || val < 0){		/* if val is greater than short max or smaller than 0 returns error token, used to keep value within range of 2-byte int */
		t.code = ERR_T;
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN);
		t.attribute.err_lex[ERR_LEN] = '\0';
	}
	else {
		t.code = INL_T;	/* sets token to integer literal */
		t.attribute.int_value = val;	/* copies value into integer value token attribute */
	}
	return t;	/* returns token */
}

/*
*	Purpose : Function is called if lexeme is determined to be an error and an error token is returned
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : strlen(), strncpy(), strcpy()
*	Parameters : Receives a char array
*	Return Value : Returns an error token
*	Algorithm : Stores lexeme into err_lex token attribute up to ERR_LEN and returns token to calling function
*/
Token aa_func12(char lexeme[]){
	Token t;
	if (strlen(lexeme) >= ERR_LEN){		/* if lexeme is greater than ERR_LEN copies only character in lexeme until ERR_LEN is reached */
		t.code = ERR_T;	/* sets token code to err token */
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN);	/* copies ERR_LEN characters from lexeme into err_lex */
		t.attribute.err_lex[ERR_LEN] = '\0';	/* adds string termination character to the end of err_lex */
	}
	else {	/* if lexeme is smaller than ERR_LEN copies full lex into err_lex and adds termination character to the end of string */
		t.code = ERR_T;
		strcpy(t.attribute.err_lex, lexeme);
		t.attribute.err_lex[strlen(lexeme)] = '\0';	/* uses strlength to determine end of string and append termination character */
	}
	return t;	/* return error token */
}

/*
*	Purpose : Function returns long value corresponding to octal integer representation of a c-string
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : strtol()
*	Parameters : Receives a pointer to the start of a char array
*	Return Value : Returns a long value
*	Algorithm : Uses strol function to convert string to long value and uses 8 as a parameter to specify base 8 as the conversion
*/
long atool(char * lexeme){
	return strtol(lexeme, NULL, 8);	/* converts c-string to octal integer using strtol and specifying base 8 in the parameters */
}

/*
*	Purpose : Function determines if contents in buffer have caused buffer to reach capacity
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : strcmp()
*	Parameters : Receives a pointer to a the start of a char array
*	Return Value : Returns an int value or R_FAIL_1 on failure
*	Algorithm : Uses a for loop to loop through keyword table and compare kwlexeme to table string, if a match is found returns the index of the
*				keyword in the table, else returns a runtime fail
*/
int iskeyword(char * kw_lexeme){
	int i;	/* iterator */
	for (i = 0; i<KWT_SIZE; i++){		/* loops through keyword table */
		if (strcmp(kw_table[i], kw_lexeme) == 0) {	/* compares kw_lexeme which each member of the keyword table */
			return i;	/* returns index of keyword in table if a match is found */
		}
	}
	return R_FAIL_1;	/* if not returns runtime fail */
}