#include <stdio.h>

int main(){

	int i, counter = 0;

	for(i=50;i>9;i-=3){
		printf("%d\n", i);
		counter+=1;
	}
	printf("\n\n\n\n%d\n",counter);
	return -1;
}