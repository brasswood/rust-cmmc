// This file was provided by Drew Davidson, with some additions by Andrew Riachi

#include "stdio.h"
#include "stdlib.h"
#include <inttypes.h>

void printBool(char c){
	if (c == 0){ 
		fprintf(stdout, "false"); 
	} else{ 
		fprintf(stdout, "true"); 
	}
	fflush(stdout);
}

void printInt(long int num){
	fprintf(stdout, "%ld", num);
	fflush(stdout);
}

void printShort(char num){
	fprintf(stdout, "%hhd", num);
	fflush(stdout);
}

void printString(const char * str){
	fprintf(stdout, "%s", str);
	fflush(stdout);
}

char getBool(){
	char c;
	scanf("%c", &c);
	getchar(); // Consume trailing newline
	if (c == '0'){
		return 0;
	} else {
		return 1;
	}
}

int64_t getInt(){
	char buffer[32];
	fgets(buffer, 32, stdin);
	long int res = atol(buffer);
	return res;
}

char getShort() {
	char buffer[32];
	fgets(buffer, 32, stdin);
	char res = (char) atoi(buffer);
	return res;
}
