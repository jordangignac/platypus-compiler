/*
*	File-Name : buffer.c
*	Author : Jordan Gignac
*	Purpose : Parser implementation for the PLATYPUS language
*/
#define _CRT_SECURE_NO_WARNINGS

#include <stdlib.h>
#include <string.h>
#include "parser.h"
/*
*	Purpose : Function initializes parser components and begins program parsing
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : Receives a pointer to a buffer object
*	Return Value : void
*/
void parser(Buffer *in_buf){
	sc_buf = in_buf;
	lookahead = mlwpar_next_token(sc_buf);
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

/*
*	Purpose : Function matches two tokens, lookahead and token required by parser
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : Receives integer pr_token_code corresponding to required code, and pr_token_attribute corresponding to required attribute
*	Return Value : void
*/
void match(int pr_token_code, int pr_token_attribute){
	int temp = 1;	/* temporary code used for storing lookahead code before match */

	switch (lookahead.code){
	case KW_T:
	case LOG_OP_T:
	case ART_OP_T:
	case REL_OP_T:
		temp = lookahead.attribute.get_int == pr_token_attribute;
		break;
	case SEOF_T:
		if (lookahead.code == pr_token_code){
			return;
		}
	}

	/* match lookahead code to parser required code */
	temp = temp && (lookahead.code == pr_token_code);

	switch (temp){
	case 1:
		if ((lookahead = mlwpar_next_token(sc_buf)).code == ERR_T){
			syn_printe();
			lookahead = mlwpar_next_token(sc_buf);
			synerrno++;
		}
		return;
	case 0:
		syn_eh(pr_token_code);
		return;
	}
}

/*
*	Purpose : Function handles errors encountered in the parser
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : Receives sync_token_code as the next token code required by parser after error
*	Return Value : void
*/
void syn_eh(int sync_token_code){
	syn_printe();	/* call error printing function */
	synerrno++;		/* increment error number */

	/* while lookahead does not equal sync code advance bufferm handling end of file within */
	while ((lookahead = mlwpar_next_token(sc_buf)).code != sync_token_code){
		if (lookahead.code == SEOF_T && sync_token_code != SEOF_T){
			exit(synerrno);
		}
	}

	if (lookahead.code = sync_token_code){
		if (lookahead.code != SEOF_T){
			lookahead = mlwpar_next_token(sc_buf);
		}
		return;
	}
}

/* Parser error printing function, Assignmet 4, F15 */
void syn_printe(){
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code){
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("NA\n");
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", sym_table.pstvr[t.attribute.get_int].plex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_setmark(str_LTBL, t.attribute.str_offset));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}
}

/*
*	Purpose : Function takes a string as an argument and prints it
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : Receives a pointer to a char array corresponding to a string
*	Return Value : void
*/
void gen_incode(char *string){
	printf("%s\n", string);
}

/*
*	Purpose : <program>->PLATYPUS{<opt_statements>}
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void program(void){
	match(KW_T, PLATYPUS);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/*
*	Purpose : <opt_statements>-><statements>
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void opt_statements(void){
	/* FIRST set: {AVID_T,SVID_T,KW_T(but not … see above),e} */
	switch (lookahead.code){
	case AVID_T:
	case SVID_T:
		statements();
		break;
	case KW_T:
		if (lookahead.attribute.kwt_idx != PLATYPUS
			&& lookahead.attribute.kwt_idx != ELSE
			&& lookahead.attribute.kwt_idx != THEN
			&& lookahead.attribute.kwt_idx != REPEAT){
			statements();
			break;
		}
	default:
		gen_incode("PLATY: Opt_statements parsed");
	}
}

/*
*	Purpose : <statements>-><statement><statements_s>
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void statements(void){
	statement();
	statements_s();
}

/*
*	Purpose : <statements_s>-><statement><statement_s>
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void statements_s(void){
	/* FIRST set: {AVID_T,SVID_T,KW_T} */
	switch (lookahead.code){
	case KW_T:
		switch (lookahead.attribute.kwt_idx){
		case PLATYPUS:
		case ELSE:
		case THEN:
		case REPEAT:
			return;
		}
	case AVID_T:
	case SVID_T:
		statement();
		statements_s();
		break;
	}
}

/*
*	Purpose : <statement>-><assignment_statement>|<selection_statement>|<iteration_statement>|<input_statement>|<output_statement>|e
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void statement(void){
	/* FIRST set: {AVID_T,SVID_T,KW_T,e} */
	switch (lookahead.code){
	case AVID_T:
	case SVID_T:
		assignment_statement();
		break;
	case KW_T:
		switch (lookahead.attribute.kwt_idx){
		case IF:
			selection_statement();
			break;
		case USING:
			iteration_statement();
			break;
		case INPUT:
			input_statement();
			break;
		case OUTPUT:
			output_statement();
			break;
		default:
			syn_printe();
		}
		break;
	default:
		syn_printe();
	}
}

/*
*	Purpose : <assignment_statement>-><assignment_expression>;
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void assignment_statement(void){
	assignment_expression(); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

/*
*	Purpose : <selection_statement>->IF(<conditional_expression>)THEN<opt_statements>ELSE {<opt_statements>};
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void selection_statement(void){
	match(KW_T, IF); match(LPR_T, NO_ATTR); conditional_expression(); match(RPR_T, NO_ATTR); match(KW_T, THEN); opt_statements();
	match(KW_T, ELSE); match(LBR_T, NO_ATTR); opt_statements(); match(RBR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: IF statement parsed");
}

/*
*	Purpose : <iteration_statement>->USING(<assignment_expression>,<conditional_expression>,<assignment_expression>)REPEAT{<opt_statements>};
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void iteration_statement(void){
	match(KW_T, USING); match(LPR_T, NO_ATTR); assignment_expression(); match(COM_T, NO_ATTR); conditional_expression(); match(COM_T, NO_ATTR); assignment_expression(); match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT); match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: USING statement parsed");
}

/*
*	Purpose : <input_statement>->INPUT(<variable_list>);
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void input_statement(void){
	match(KW_T, INPUT); match(LPR_T, NO_ATTR); variable_list(); match(RPR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: INPUT statement parsed");
}

/*
*	Purpose : <output_statement>->OUTPUT(<output_statement_s>);
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void output_statement(void){
	match(KW_T, OUTPUT); match(LPR_T, NO_ATTR); output_statement_s(); match(RPR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: OUTPUT statement parsed");
}

/*
*	Purpose : <output_statement_s>-><opt_variable_list>|STR_T
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void output_statement_s(void){
	/* FIRST set: {AVID_T,SVID_T,STR_T} */
	switch (lookahead.code){
	case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
		break;
	case AVID_T:
	case SVID_T:
		opt_variable_list();
		break;
	default:
		gen_incode("PLATY: Output list (empty) parsed");
	}
}

/*
*	Purpose : <opt_variable_list>-><variable_list>|e
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void opt_variable_list(void){
	/* FIRST set: {AVID_T,SVID_T,e} */
	switch (lookahead.code){
	case AVID_T:
	case SVID_T:
		variable_list();
		break;
	default:
		syn_printe();
	}
}

/*
*	Purpose : <variable_list>-><variable_identifier><variable_list_s>
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void variable_list(void){
	variable_identifier();
	variable_list_s();
	gen_incode("PLATY: Variable list parsed");
}

/*
*	Purpose : <variable_list_s>->,<variable_identifier><variable_list_s>
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void variable_list_s(void){
	/* FIRST set: {,} */
	switch (lookahead.code){
	case COM_T:
		match(COM_T, NO_ATTR); variable_identifier(); variable_list_s();
		break;
	}
}

/*
*	Purpose : <variable_identifier>->AVID_T|SVID_T|e
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void variable_identifier(void){
	/* FIRST set: {AVID_T,SVID_T,e} */
	switch (lookahead.code){
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;
	default:
		syn_printe();
	}
}

/*
*	Purpose : <assignment_expression>->AVID_T=<arithmetic_expression>|SVID_T=<string_expression>|e
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void assignment_expression(void){
	/* FIRST set: {AVID_T,SVID_T,==,e} */
	switch (lookahead.code){
	case AVID_T:
		match(AVID_T, NO_ATTR); match(ASS_OP_T, NO_ATTR); arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR); match(ASS_OP_T, NO_ATTR); string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	default:
		syn_printe();
	}
}

/*
*	Purpose : <conditional_expression>-><logical_or_expression>
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void conditional_expression(void){
	logical_or_expression();
	gen_incode("PLATY: Conditional expression parsed");
}

/*
*	Purpose : <logical_or_expression>-><logical_and_expression><logical_or_expression_s>
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void logical_or_expression(void){
	logical_and_expression();
	logical_or_expression_s();
}

/*
*	Purpose : <logical_or_expression_s>->.OR.<logical_and_expression><logical_or_expression_s>
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void logical_or_expression_s(void){
	/* FIRST set: {.OR.} */
	switch (lookahead.code){
	case LOG_OP_T:
		if (lookahead.attribute.log_op == OR){
			match(LOG_OP_T, OR); logical_and_expression(); logical_or_expression_s();
			gen_incode("PLATY: Logical OR expression parsed");
			break;
		}
	}
}

/*
*	Purpose : <logical_and_expression>-><relational_expression><logical_and_expression_s>
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void logical_and_expression(void){
	relational_expression();
	logical_and_expression_s();
}

/*
*	Purpose : <logical_and_expression_s>->.AND.<relational_expression><logical_and_expression_s>
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void logical_and_expression_s(void){
	/* FIRST set: {.AND.} */
	switch (lookahead.code){
	case LOG_OP_T:
		if (lookahead.attribute.log_op == AND){
			match(LOG_OP_T, AND); relational_expression(); logical_and_expression_s();
			gen_incode("PLATY: Logical AND expression parsed");
			break;
		}
	}
}

/*
*	Purpose : <relational_expression>-><primary_a_relational_expression><primary_a_relational_expression_s>|<primary_s_relational_expression><primary_s_relational_expression_s>
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void relational_expression(void){
	/* FIRST set: {AVID_T,FPL_T,INL_T,SVID_T,STR_T,e} */
	switch (lookahead.code){
	case AVID_T:
	case FPL_T:
	case INL_T:
		primary_a_relational_expression();
		primary_a_relational_expression_s();
		break;
	case SVID_T:
	case STR_T:
		primary_s_relational_expression();
		primary_s_relational_expression_s();
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Relational expression parsed");
}

/*
*	Purpose : <primary_a_relational_expression>->AVID_T|FPL_T|INL_T|e
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void primary_a_relational_expression(void){
	/* FIRST set: {AVID_T, FPL_T, INL_T, e} */
	switch (lookahead.code){
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		break;
	case INL_T:
		match(INL_T, NO_ATTR);
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}

/*
*	Purpose : <primary_a_relational_expression_s>->==<primary_a_relational_expression>|!=<primary_a_relational_expression>|><primary_a_relational_expression>|<<primary_a_relational_expression>|e
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void primary_a_relational_expression_s(void){
	/* FIRST set: {==,!=,>,<,e} */
	switch (lookahead.attribute.rel_op){
	case EQ:
		match(REL_OP_T, EQ);
		primary_a_relational_expression();
		break;
	case NE:
		match(REL_OP_T, NE);
		primary_a_relational_expression();
		break;
	case GT:
		match(REL_OP_T, GT);
		primary_a_relational_expression();
		break;
	case LT:
		match(REL_OP_T, LT);
		primary_a_relational_expression();
		break;
	default:
		syn_printe();
	}
}

/*
*	Purpose : <primary_s_relational_expression>-><primary_string_expression>
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void primary_s_relational_expression(void){
	primary_string_expression();
	gen_incode("PLATY: Primary s_relational expression parsed");
}

/*
*	Purpose : <primary_s_relational_expression_s>->==<primary_s_relational_expression>|!=<primary_s_relational_expression>|><primary_s_relational_expression>|<<primary_s_relational_expression>|e
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void primary_s_relational_expression_s(void){
	/* FIRST set: {==,!=,>,<,e} */
	switch (lookahead.attribute.rel_op){
	case EQ:
		match(REL_OP_T, EQ);
		primary_s_relational_expression();
		break;
	case NE:
		match(REL_OP_T, NE);
		primary_s_relational_expression();
		break;
	case GT:
		match(REL_OP_T, GT);
		primary_s_relational_expression();
		break;
	case LT:
		match(REL_OP_T, LT);
		primary_s_relational_expression();
		break;
	default:
		syn_printe();
	}
}

/*
*	Purpose : <string_expression>-><primary_string_expression><string_expression_s>
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void string_expression(void){
	primary_string_expression();
	string_expression_s();
	gen_incode("PLATY: String expression parsed");
}

/*
*	Purpose : <string_expression_s>-><<<primary_string_expression><string_expression_s>
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void string_expression_s(void){
	/* FIRST set: {<<} */
	switch (lookahead.code){
	case SCC_OP_T:
		match(SCC_OP_T, NO_ATTR); primary_string_expression(); string_expression_s();
		break;
	}
}

/*
*	Purpose : <primary_string_expression>->STR_T|SVID_T|e
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void primary_string_expression(void){
	/* FIRST set: {STR_T, SVID_T, e} */
	switch (lookahead.code){
	case STR_T:
		match(STR_T, NO_ATTR);
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Primary string expression parsed");
}

/*
*	Purpose : <arithmetic_expression>-><unary_arithmetic_expression>|<additive_arithmetic_expression>
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void arithmetic_expression(void){
	/* FIRST set: {*,/,AVID_T,FPL_T,INL_T,(,e} */
	switch (lookahead.code){
	case ART_OP_T:
		if (lookahead.attribute.arr_op == MULT || lookahead.attribute.arr_op == DIV){
			syn_printe();
			return;
		}
		unary_arithmetic_expression();
		break;
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		additive_arithmetic_expression();
		break;
	default:
		syn_printe();
		return;
	}
	gen_incode("PLATY: Arithmetic expression parsed");
}

/*
*	Purpose : <unary_arithmetic_expression>->+<primary_arithmetic_expression>|-<primary_arithmetic_expression>
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void unary_arithmetic_expression(void){
	/* FIRST set: {+,-,e} */
	switch (lookahead.code){
	case ART_OP_T:
		switch (lookahead.attribute.arr_op){
		case PLUS:
			match(ART_OP_T, PLUS);
			break;
		case MINUS:
			match(ART_OP_T, MINUS);
			break;
		default:
			syn_printe();
			return;
		}
		primary_arithmetic_expression();
		gen_incode("PLATY: Unary arithmetic expression parsed");
		break;
	default:
		syn_printe();
	}
}

/*
*	Purpose : <additive_arithmetic_expression>-><multiplicative_arithmetic_expression><additive_arithmetic_expression_s>
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void additive_arithmetic_expression(void){
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_s();
}

/*
*	Purpose : <additive_arithmetic_expression_s>->+<multiplicative_arithmetic_expression><additive_arithmetic_expression_s|-<multiplicative_arithmetic_expression><additive_arithmetic_expression_s>
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void additive_arithmetic_expression_s(void){
	/* FIRST set: {+,-} */
	switch (lookahead.code){
	case ART_OP_T:
		if (lookahead.attribute.arr_op == PLUS){
			match(ART_OP_T, PLUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_s();
		}
		else if (lookahead.attribute.arr_op == MINUS){
			match(ART_OP_T, MINUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_s();
		}
		gen_incode("PLATY: Additive arithmetic expression parsed");
	}
}

/*
*	Purpose : <multiplicative_arithmetic_expression>-><primary_arithmetic_expression><multiplicative_arithmetic_expression_s>
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void multiplicative_arithmetic_expression(void){
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_s();

}

/*
*	Purpose : <multiplicative_arithmetic_expression_s>->*<primary_arithmetic_expression><multiplicative_arithmetic_expression_s>|/<primary_arithmetic_expression><multiplicative_arithmetic_expression_s>
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void multiplicative_arithmetic_expression_s(void){
	/* FIRST set: {*, /} */
	switch (lookahead.code){
	case ART_OP_T:
		if (lookahead.attribute.arr_op == MULT){
			match(ART_OP_T, MULT); primary_arithmetic_expression(); multiplicative_arithmetic_expression_s();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		}
		else if (lookahead.attribute.arr_op == DIV){
			match(ART_OP_T, DIV); primary_arithmetic_expression(); multiplicative_arithmetic_expression_s();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		}
	}
}

/*
*	Purpose : <primary_arithmetic_expression>->AVID_T|FPL_T|INL_T|(<arithmetic_expression>)|e
*	Author : Jordan Gignac
*	Version : 1.0
*	Parameters : void
*	Return Value : void
*/
void primary_arithmetic_expression(void){
	/* FIRST set: {AVID_T,FPL_T,INL_T,(,e} */
	switch (lookahead.code){
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		break;
	case INL_T:
		match(INL_T, NO_ATTR);
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR); arithmetic_expression(); match(RPR_T, NO_ATTR);
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}