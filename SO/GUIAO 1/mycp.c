#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>

int mycp(char* input_file,char* output_file)
{
	
#define SIZE 1

	//abrir o ficheiro de entrada

	int fd_in = open(input_file,O_RDONLY);
	if(fd_in < 0) {
		perror("erro ao abrir ficheiro de entrada");
		return -1;

	}

	//abrir o ficheiro de saida

	int fd_out = open(output_file,O_WRONLY);
	if(fd_out<0) {
		perror("erro ao abrir ficheiro de saida");
		return -1;
	}


	printf("fd_in = %d, fd_out = %d,")



	//ler do ficheiro de input


	char buf[SIZE];

	

	ssize_t bytes_read;

	while((bytes_read == read(fd_in,buf,SIZE)));
	
	printf("li %zd bytes\n",bytes_read);

	ssize_t bytes_written = write(fd_out,buf,bytes_read);

	//fechar ficheiro de entrada

	close(fd_in);

	//fechar ficheiro de saida

	close(fd_out);




}



int main(int argc, char** argv)
{
	int i;

	printf("argc: %d\n", argc);

	for(i=0;i<argc;i++)
	{
		printf("argv[%d] = %s\n", i,argc);
	}

	
	return 0;
}