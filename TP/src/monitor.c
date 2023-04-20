#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <limits.h>

int main(int argc, char *argv[])
{
	int read_bytes;

	char buffer[1024];

	char* ficheiro = "tmp/ficheiro.txt";


	// Abrir fifo //

	int fif = mkfifo("fifo",0666);

	if(fif == -1)
	{
		perror("Erro ao abrir Fifo");
		return -1;
	}




	int fdCliente = open("fifo",O_RDONLY);

	int fdFile = open(ficheiro, O_WRONLY | O_CREAT | O_APPEND, 0600);


	while((read_bytes = read(fdCliente, buffer, 1024)) > 0)
	{
		//write(1,buffer,read_bytes);

		// Escreve para o ficheiro "ficheiro.txt"
		write(fdFile, buffer, read_bytes);
	}
	


	

	close(fdCliente);

	close(fdFile);

	unlink("fifo");

	return 0;
}

/*void runStatus()
	{
		pid_t f = fork();

		if(f == 0)
		{
			printf("PID\nNome\nTempo de execução até ao momento\n");
		}
	}*/
		