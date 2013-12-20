/* Aftab Khan */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define BUF_SIZE 1024

void printNTabs(int n){
	for (int i = 0; i < n; ++i)
	{
		printf(" ");
	}
}


int main (int argc, char** argv){
	char buffer[BUF_SIZE];
	char lineBuffer[BUF_SIZE + 2];
	char* tabCharSet = "\t "; // Space, Tab characters
	size_t contentSize = 1; // includes NULL

	char *content = malloc(sizeof(char) * BUF_SIZE);
	if(content == NULL){
		perror("Failed to allocate content");
		exit(1);
	}

	content[0] = '\0'; // make null-terminated
	
	int oldTabLength = -1;
	int tabLength = 0;

	while(fgets(buffer, BUF_SIZE, stdin)){

		//printf("READ: %s", buffer);
		oldTabLength = tabLength;
		tabLength = strspn(buffer, tabCharSet);
		//printf("\ttabLength = %i, oldTabLength = %i\n", tabLength, oldTabLength);
		if(tabLength > oldTabLength){
			//strcat(buffer, "{\n");
			printNTabs(oldTabLength);
			printf("{\n%s", buffer);
		} else if (tabLength < oldTabLength){
			//strcat(buffer, "}\n");
			printNTabs(tabLength);
			printf("}\n%s", buffer);
		} else printf("%s", buffer);
		//printf("%s\n", buffer);
		

		//TO BE USED IF OUTPUTTING TO A FILE IS DESIRED:
		/*char *old = content;
		contentSize += strlen(buffer);
		content = realloc(content, contentSize);
		if(content == NULL){
			perror("Failed to reallocate content");
			free(old);
			exit(2);
		}
		strcat(content, buffer);*/

	}
	if (oldTabLength > -1) printf("\n}\n");

	if(ferror(stdin)){
		free(content);
		perror("Error reading from stdin");
		exit(3);
	}
}