/* Author: Aftab Khan */

/* 
 * Reads SPWAG source files (without curly braces "{}") from Standard In
 * and inserts curly braces as appropriate. Preprocessed SPWAG code is
 * returned to calling OCaml function as a String.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <regex.h>
#include <sys/types.h>
#include <caml/alloc.h>
//#include <caml/memory.h>
#include <caml/mlvalues.h>

#define BUF_SIZE 1024

 void addNTabs(int n, char *destination){
 	int i;
 	for (i = 0; i < n; ++i){
 		strcat(destination, " ");
 	}
 }


 int isSpecialSyntax(char *lineString, int tabLength){
 	regex_t regex;
 	int result;
	//result = regcomp(&regex, "(define )|((if|while)[' ']*\()", REG_EXTENDED);
	//result = regcomp(&regex, "(define )|(if|while) |(if|while)\(", REG_EXTENDED);
 	result = regcomp(&regex, "^(	| )*((define )|(if |while )|(if\\(|while\\())", REG_EXTENDED);
 		if (result) {fprintf(stderr, "Could not compile regex\n"); exit(1);}

	char *trimmedString = lineString; //+ tabLength;
	//printf("trimmedstring: %s", trimmedString);

	result = regexec(&regex, trimmedString, 0, NULL, 0);
	//if ((NULL != strchr(trimmedString, '(')) && result){
	if (result && (strlen(trimmedString) > 1)){
		//printf("unusual box instance syntax\n");
		return 1;
/*	} else if (result == REG_NOMATCH){
        return 0; */
	} else {
        //fprintf(stderr, "Regex match failed");
		return 0;
        //exit(1);
	}
}


// corresponds to OCaml function type: unit -> String
CAMLprim value caml_preprocess(value unit)
{
	char buffer[BUF_SIZE];
	char lineBuffer[BUF_SIZE];

	// Valid Tab Characters: Tab (\t), Space
	char *tabCharSet = "\t ";

	size_t contentSize = 1; // includes NULL

	char *content = malloc(sizeof(char) * BUF_SIZE);
	if(content == NULL){
		perror("Failed to allocate content");
		exit(1);
	}

	content[0] = '\0'; // make null-terminated

	int oldTabLength = -1;
	int tabLength = 0;
	int specialSyntaxFlag = 0;

	while(fgets(lineBuffer, BUF_SIZE, stdin)){

		buffer[0] = '\0';

		oldTabLength = tabLength;
		tabLength = strspn(lineBuffer, tabCharSet);

		if(tabLength > oldTabLength){
			if (specialSyntaxFlag){
				// Special component instance syntax
				content[strlen(content) - 1] = '\0';
				strcat(buffer, "{\n");
				specialSyntaxFlag = 0;
			} else {
				addNTabs(oldTabLength, buffer);
				strcat(buffer, "{\n");
			}
		} else if (tabLength < oldTabLength){
			addNTabs(tabLength, buffer);
			strcat(buffer, "}\n");
		}
		strcat(buffer, lineBuffer);

		// updates special syntax flag
		specialSyntaxFlag = isSpecialSyntax(lineBuffer, tabLength);

		char *old = content;
		contentSize += strlen(buffer);
		content = realloc(content, contentSize);
		if(content == NULL){
			perror("Failed to reallocate content");
			free(old);
			exit(2);
		}
		strcat(content, buffer);

	}

	// Prints the final curly brace, if the file contained any lines
	if (oldTabLength > -1) strcpy(buffer, "\n}\n\0");
	char *old = content;
	contentSize += strlen(buffer);
	content = realloc(content, contentSize);
	if(content == NULL){
		perror("Failed to reallocate content");
		free(old);
		exit(2);
	}
	strcat(content, buffer);

	if(ferror(stdin)){
		free(content);
		perror("Error reading from stdin");
		exit(3);
	}

	//printf("Result\n\n%s\n", content);
	return caml_copy_string(content);
}
