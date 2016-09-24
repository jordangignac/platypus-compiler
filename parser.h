/*
*	File-Name : buffer.c
*	Author : Jordan Gignac
*	Purpose : Stores all constant definitions, function declarations and declarations which are required for the implementation of the symbol table
*/

/* header guard */
#ifndef PARSER_H_
#define PARSER_H_

/* includes */
#include "buffer.h"
#include "token.h"
#include "stable.h"

/* static global variables */
static Token lookahead;
static Buffer *sc_buf;

/* kwt constants */
#define NO_ATTR 0
#define ELSE 0
#define IF 1
#define INPUT 2
#define OUTPUT 3
#define PLATYPUS 4
#define REPEAT 5
#define THEN 6
#define USING 7
#define KWT_SIZE 8

/* external scanner global variables */
extern Token mlwpar_next_token(Buffer *);
extern int line;
extern STD sym_table;
extern Buffer *str_LTBL;
extern char *kw_table[KWT_SIZE];

/* global variables */
int synerrno;

/* function prototypes */
void parser(Buffer *in_buf);
void match(int pr_token_code, int pr_token_attribute);
void syn_eh(int sync_token_code);
void syn_printe();
void gen_incode(char *string);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_p(void);
void arithmetic_expression(void);
void assignment_expression(void);
void assignment_statement(void);
void conditional_expression(void);
void input_statement(void);
void iteration_statement(void);
void logical_and_expression(void);
void logical_and_expression_p(void);
void logical_or_expression(void);
void logical_or_expression_p(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_p(void);
void opt_statements(void);
void opt_variable_list(void);
void output_statement(void);
void primary_a_relational_expression(void);
void primary_arithmetic_expression(void);
void primary_s_relational_expression(void);
void primary_string_expression(void);
void program(void);
void relational_expression(void);
void relational_expression_p(void);
void relational_expression_p_str(void);
void selection_statement(void);
void statement(void);
void statements(void);
void statements_p(void);
void string_expression(void);
void string_expression_p(void);
void unary_arithmetic_expression(void);
void variable_identifier(void);
void variable_list(void);
void variable_list_p(void);

#endif	/* end header guard */
