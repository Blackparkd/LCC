#include <stdio.h>
#include <stdlib.h>
#include "inc.h"


int inc(int i){
	i++;
	return i;
}

int main(int argc, char* argv[]){
	
	printf("valor: %d \n",inc(atoi(argv[1])));

	return 0;
} 
