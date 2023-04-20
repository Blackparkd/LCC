#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>

#define BUF_SIZE 1024

int main(int argc, char* argv[]){

	int bytes_read;
	char *buf = (char*) malloc(sizeof(char) * BUF_SIZE);

	while((bytes_read = read(0,buf,BUF_SIZE)) > 0) {
		write(1, buf, bytes_read);
	}
	printf("received EOF\n");

	free(buf);

	return 0;

}


