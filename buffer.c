/*
*	File-Name : buffer.c
*	Author : Jordan Gignac
*	Purpose : Take an input stream of characters in the form of an input file and store them in a buffer,
*			  featuring several operating modes and capacities.
*/
#define _CRT_SECURE_NO_WARNINGS

#include "buffer.h"
/*
*	Purpose : This function creates a new buffer in memory, and sets members to appropriate values
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : n/a
*	Parameters : Receives short for initial capacity of the buffer, char for determining icrement value, and a char for operational mode of buffer
*	Return Value : Returns a pointer to the newly created and allocated buffer
*	Algorithm : Uses inputs from external source to set corresponding values and operating modes of buffer
*/
Buffer * b_create(short init_capacity, char inc_factor, char o_mode)
{
	/* Define and initialize buffer struct, dynamically allocate memory for struct and char pointer */
	Buffer *buffer = (Buffer *)calloc(1, sizeof(Buffer));
	buffer->cb_head = (char *)malloc(init_capacity);
	buffer->addc_offset = 0;
	buffer->getc_offset = 0;
	buffer->capacity = init_capacity;

	/* Check input parameter to determine operation mode of buffer, and proceed accordingly; returns null if parameter is invalid */
	if (o_mode == 'f') {
		buffer->mode = B_OP_MODE_F;
		buffer->inc_factor = 0;
	}
	else if (o_mode == 'a' && inc_factor > 0 && inc_factor < 256) {
		buffer->mode = B_OP_MODE_A;
		buffer->inc_factor = inc_factor;
	}
	else if (o_mode == 'm' && inc_factor > 0 && inc_factor <= 100) {
		buffer->mode = B_OP_MODE_M;
		buffer->inc_factor = inc_factor;
	}
	else return NULL;
	/* return buffer upon successful creation */
	return buffer;
}

/*
*	Purpose : Function attempts to add character to buffer, and resize capacity if needed depending on buffer operational mode
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : b_isfull(), sizeof()
*	Parameters : Receives a pointer to a buffer structure, and a char type referencing the character to be added to the buffer
*	Return Value : Returns a pointer to the updated buffer structure
*	Algorithm : Checks if buffer has reached capacity, if so and buffer is not in fixed operation mode it attempts to resize the current capacity.  If the function is succesfull in resizing the buffer capacity it reallocates the buffer on memory to
account for the updated size. If there is space for the character to be entered within the constraints on the buffer size created by the datatype, then it is added to the buffer and a pointer to the buffer is returned else returns null
*/
pBuffer b_addc(pBuffer const pBD, char symbol)
{
	pBD->r_flag = 0;	/* resets reallocation flag */
	if (pBD) {
		if (b_isfull(pBD)) {
			/* local variables to be used in capacity calculations */
			char *currentLocation = pBD->cb_head;
			short availCap = (BUFFER_MAX_SIZE - pBD->capacity);
			short newCap = 0, newInc = 0;

			if (availCap <= 0){ return NULL; }

			switch (pBD->mode) {
			case B_OP_MODE_F:
				return NULL;
			case B_OP_MODE_A:
				newCap = (pBD->capacity + ((pBD->inc_factor * sizeof(char))));
				if (newCap > (BUFFER_MAX_SIZE - pBD->inc_factor)){ return NULL; }
				break;
			case B_OP_MODE_M:
				newInc = (availCap * pBD->inc_factor) / 100;
				newCap = pBD->capacity + newInc;
				if (newCap == pBD->capacity){ newCap = BUFFER_MAX_SIZE; }
				if (newCap > (BUFFER_MAX_SIZE - newInc)){ return NULL; };
				break;
			default:
				return NULL;
			}
			/* reallocates memory for char buffer with new capacity */
			pBD->cb_head = (char *)realloc(pBD->cb_head, newCap);
			/* determines if the cb_head buffer has been reallocated and moved position in memory; sets r_flag appropriately */
			if (currentLocation != pBD->cb_head) { pBD->r_flag = SET_R_FLAG_1; }
			/* sets buffer capacity to the newly expanded capacity*/
			pBD->capacity = newCap;
		}
		/* copies symbol into buffer using add char offset, then increments this value */
		pBD->cb_head[pBD->addc_offset] = symbol;
		pBD->addc_offset++;
		return pBD;
	}
	else return NULL;
}

/*
*	Purpose :  Resets all members of the buffer structure while maintaing memory allocation
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : n/a
*	Parameters : Receives a pointer to a buffer structure
*	Return Value : Returns 1 upon successful reset or runtime fail
*	Algorithm : n/a
*/
int b_reset(Buffer * const pBD)
{
	/* buffer must exist to be resetable*/
	if (pBD) {
		/* resets buffer data members to 0*/
		pBD->addc_offset = 0;
		pBD->getc_offset = 0;
		pBD->mark_offset = 0;
		pBD->r_flag = 0;
		return SUCCESS;
	}
	else return R_FAIL_1;
}

/*
*	Purpose : This function deallocates the memory occupied by the char buffer and the buffer struct
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : free()
*	Parameters : Receives a pointer to buffer structure
*	Return Value : n/a
*	Algorithm : n/a
*/
void b_destroy(Buffer * const pBD)
{
	/* buffer must exist to be destructible */
	if (pBD) {
		/* buffer char pointer must exist before it can be freed */
		if (pBD->cb_head) {
			free(pBD->cb_head);
		}
		/* frees buffer struct after buffer has been freed */
		free(pBD);
	}
}

/*
*	Purpose : Function determines if contents in buffer have caused buffer to reach capacity
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : sizeof(), b_size()
*	Parameters : Receives a pointer to a buffer structure
*	Return Value : Returns true or false depending on the status of the buffer
*	Algorithm : Uses bsize to determine the number of characters in the buffer, then uses size of to determine memory space and compare with buffer capacity to determine if buffer is full
*/
int b_isfull(Buffer * const pBD)
{
	/* determines if buffer is full by comparing size of characters occupying array to capacity */
	if ((sizeof(char)* b_size(pBD)) >= pBD->capacity) {
		return B_TRUE;
	}
	else return B_FALSE;
}

/*
*	Purpose : Returns the size of the current buffer measured in characters
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : n/a
*	Parameters : Receives a pointer to a buffer structure
*	Return Value : Returns the add character of the buffer corresponding to the amount of space measured in chars being used in the buffer
*	Algorithm :
*/
short b_size(Buffer * const pBD)
{
	if (pBD) {
		/* returns the number of characters occupying array using add char offset */
		return pBD->addc_offset;
	}
	else return R_FAIL_1;
}

/*
*	Purpose : Returns the current capacity of the buffer to the calling function
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : n/a
*	Parameters : Receives a pointer to a buffer structure
*	Return Value : Returns the current capacity of the buffer structure to the calling function
*	Algorithm : n/a
*/
short b_capacity(Buffer * const pBD)
{
	if (pBD) {
		/* simply returns the capacity of the buffer using the data member capacity */
		return pBD->capacity;
	}
	else return R_FAIL_1;
}

/*
*	Purpose : Sets the current mark of the buffer to an updated value
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : n/a
*	Parameters : Receives a pointer to a buffer structure and a short corresponding to the new mark
*	Return Value : Returns a pointer to the currently set mark offset location in the buffer character array or null if fail
*	Algorithm : Makes sure mark is within the limits of the buffer, if so sets the buffers mark offset to the new value, and attempts to return pointer to mark location in array
*/
char * b_setmark(Buffer * const pBD, short mark)
{
	if (pBD) {
		/* checks to make sure mark is within the bounds of the buffer */
		if (mark >= 0 && mark <= pBD->addc_offset) {
			/* sets buffer mark offset to the mark external parameter */
			pBD->mark_offset = mark;
			return &pBD->cb_head[pBD->mark_offset];
		}
		else return NULL;
	}
	return NULL;
}

/*
*	Purpose : Returns the mark offset of the buffer to the calling function
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : n/a
*	Parameters : Receives a pointer to a buffer structure
*	Return Value : Returns the mark offset of the buffer to the calling function in the form of a short
*	Algorithm : n/a
*/
short b_mark(Buffer * const pBD)
{
	if (pBD) {
		/* returns mark offset of buffer by returning value in mark_offset data member */
		return pBD->mark_offset;
	}
	else return R_FAIL_1;
}

/*
*	Purpose : Returns the current operating mode of the buffer to the calling function
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : n/a
*	Parameters : Receives a pointer to a buffer structure
*	Return Value : Returns the operating mode of the buffer in integer form ranging from -1 to 1
*	Algorithm : n/a
*/
int b_mode(Buffer * const pBD)
{
	if (pBD) {
		/* returns buffer operation mode by returning value in mode data member */
		return pBD->mode;
	}
	else return R_FAIL_1;
}

/*
*	Purpose : Returns the increment factor to the calling function
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : n/
*	Parameters : Receives a pointer to a buffer structure
*	Return Value : Returns the buffer structures increment value
*	Algorithm : n/a
*/
size_t b_inc_factor(Buffer * const pBD)
{
	if (pBD) {
		/* returns increment factor of buffer by returning value in inc_factor data member */
		return pBD->inc_factor;
	}
	else return 256;
}

/*
*	Purpose : Function reads an open input file into the buffer
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : feof(), fgetc(), b_addc()
*	Parameters : Receives a pointer to an input file, and a pointer to a buffer structure
*	Return Value : Returns the number of characters added to the buffer is succesfull else runtime fail
*	Algorithm : Loops through open input file checking for eof from he FILE object, if the character received is not the EOF character then the function attempts to add it to the buffer, if succesfull increases character count, else returns load fail
*/
int b_load(FILE * const fi, Buffer * const pBD)
{
	/* local variable for returning number of char */
	int numChar = 0;
	/* checks if the filestream exists */
	if (fi) {
		/* loop while not end of file */
		while (!feof(fi)) {
			/* define local variable for char */
			char tmp;
			/* determine if its actually the end of the file because feof can be picky? */
			tmp = fgetc(fi);
			if (tmp == EOF) { break; }
			/* attempts to add char to buffer, if any error is experienced returns load fail */
			if (!b_addc(pBD, tmp)) {
				return LOAD_FAIL;
			}
			else { numChar++; }
		}
		return numChar;
	}
	else return R_FAIL_1;
}

/*
*	Purpose : Determines if the buffer is currently empty
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : n/a
*	Parameters : Receives a pointer to a buffer structure
*	Return Value : Returns true if the buffer is empty and false if it is not
*	Algorithm : Uses the add character offset element of the structure to determine if the buffer is empty because if this is 0 then the next added element must be the first and therefore the buffer is empty
*/
int b_isempty(Buffer * const pBD)
{
	if (pBD) {
		/* checks if the buffer is empty by analyzing add char offset, which would define the buffer as empty if 0 */
		if (pBD->addc_offset == 0) {
			return B_TRUE;	/* return true */
		}
		else return B_FALSE;	/* return false */
	}
	else return R_FAIL_1;
}

/*
*	Purpose : Returns the eob value to calling function
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : n/a
*	Parameters : Receives a pointer a buffer structure
*	Return Value : Returns the end of buffer integer value to the calling function
*	Algorithm : n/a
*/
int b_eob(Buffer * const pBD)
{
	if (pBD) {
		/* returns end of buffer data member to calling function */
		return pBD->eob;
	}
	else return R_FAIL_1;
}

/*
*	Purpose : Checks argument for validity, checks to make sure end of buffer has not been reached, else returns the character located at the current get character offset location in the buffer
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : n/a
*	Parameters : Receives a pointer to a buffer structure
*	Return Value : Returns the character located at the current get character offset of the buffer to the calling function, or run time fail
*	Algorithm : Compares the get character and add character to determine if the end of the buffer has been reached, if not used that get character offset to return the character at that location before incrementing the get character offset value
*/
char b_getc(Buffer * const pBD)
{
	/* check if the buffer exists before attempting to get char */
	if (pBD) {
		/* check if end of buffer has been reached by comparing get char offset to the add char offset*/
		if (pBD->getc_offset == pBD->addc_offset) {
			pBD->eob = 1;	/* sets buffer data member apprpriately */
			return R_FAIL_1;
		}
		else {
			return pBD->cb_head[pBD->getc_offset++];	/* returns the char corresponding to the value at the get char offset, then incrementing it */
		}
	}
	else return R_FAIL_2;
}

/*
*	Purpose : Intended for diagnostic purposes to display buffer neatly to stdout
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functins : b_size(), b_eob()
*	Parameters : Receives a pointer to a buffer structure
*	Return Value : Returns the number of characters printed to stdout or runtime fail
*	Algorithm : Loops through characters in buffer until end of buffer flag is reached, end of buffer is checked again directly before printing character to prevent overprinting characters despite reaching end of buffer
*/
int b_print(Buffer * const pBD)
{
	/* local variable for storing char to print */
	char c_get;
	/* determines if the buffer is empty before attempting to print */
	if (b_size(pBD) == 0) {
		printf("The buffer is empty.");
	}
	/* sets the get char offset to 0, or the start of the buffer */
	pBD->getc_offset = 0;

	/* loops through buffer until EOB is reached*/
	while (b_eob(pBD) == 0){
		c_get = b_getc(pBD);
		/* explicitly recheck if the end of buffer has been reached before actually printing character */
		if (b_eob(pBD) == 1){ break; }
		/* print character to console */
		printf("%c", c_get);
	}

	/* prints new line for formatting sake */
	printf("\n");
	return pBD->addc_offset;
}

/*
*	Purpose : This function shrinks or expands the buffer to a new capacity, which is the current size with space for an additional char
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : sizeof()
*	Parameters : Receives a pointer to a buffer structure
*	Return Value : Returns a pointer to the newly resized buffer or NULL upon failure
*	Algorithm : Functions makes sure buffer can safely be resized within constraints, attempts to add appropriate space to capacity, dynamically reallocate memory for the buffer with the new capacity,
sets reallocation flag and resets buffer members before returning a pointer to the structure to the calling function
*/
Buffer * b_pack(Buffer * const pBD)
{
	/* checks if buffer exists before attempting to pack */
	if (pBD) {
		/* checks to make sure another char can be added without overstepping MAX_BUFFER_SIZE */
		if (pBD->capacity > (BUFFER_MAX_SIZE - pBD->inc_factor)){ return NULL; }

		/* sets capacity equal to size of the buffer + 1 element */
		pBD->capacity = sizeof(char)* (pBD->addc_offset + 1);

		/* reallocates buffer with updated capacity */
		pBD->cb_head = (char *)realloc(pBD->cb_head, pBD->capacity);

		/* sets the buffer reallocation flag appropriately */
		pBD->r_flag = SET_R_FLAG_1;

		/* resets buffer data members */
		pBD->getc_offset = 0;
		pBD->eob = 0;

		return pBD;
	}
	else return NULL;
}

/*
*	Purpose : Used to return the r_flag in a function call
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : n/a
*	Parameters : Receives a pointer to a buffer structure
*	Return Value : Returns the reallocaton flag of the buffer structure
*	Algorithm : n/a
*/
char b_rflag(Buffer * const pBD)
{
	if (pBD) {
		/* returns reallocation flag data member from buffer struct */
		return pBD->r_flag;
	}
	else return R_FAIL_1;
}

/*
*	Purpose : Decrements the current get character offset of the buffer by one
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : n/a
*	Parameters : Receives a pointer to a buffer structure
*	Return Value : Returns the newly decremented get character offset or runtime fail
*	Algorithm : n/a
*/
short b_retract(Buffer * const pBD)
{
	if (pBD) {
		/* decrements get character offset; retracting the offset by one element */
		return --pBD->getc_offset;
	}
	else return R_FAIL_1;
}

/*
*	Purpose : Sets the get character offset for the buffer and sets it to the current mark offset
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : n/a
*	Parameters : Receive a pointer to a buffer structure
*	Return Value : Returns the newly edited get character offset or runtime fail
*	Algorithm : n/a
*/
short b_retract_to_mark(Buffer * const pBD)
{
	if (pBD) {
		/* sets the get char offset to the mark offset */
		pBD->getc_offset = pBD->mark_offset;
		return pBD->getc_offset;
	}
	else return R_FAIL_1;
}

/*
*	Purpose : Returns the getc_offset value of the bufer to the calling function
*	Author : Jordan Gignac
*	Version : 1.0
*	Called Functions : n/a
*	Parameters : Receives a pointer to a buffer structure
*	Return Value :	Returns the get character offset of the buffer structure
*	Algorithm : n/a
*/
short b_getc_offset(Buffer * const pBD)
{
	if (pBD) {
		/* returns get character offset of the buffer struct using the getc_offset data member */
		return pBD->getc_offset;
	}
	else return R_FAIL_1;
}

