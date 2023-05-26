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
	int res;

	char buffer[100];

	int fdpass = open("/etc/passwd", O_RDONLY);

	int fdsaida = open("saida.txt", O_CREAT | O_TRUNC | O_WRONLY, 0600);
	int fderr =  open("erros.txt", O_CREAT | O_TRUNC | O_WRONLY, 0600);

	/*dup2(fdpass, 0);
	close(fdpass);

	dup2(fdsaida, 1);
	close(fdsaida);

	dup2(fderr, 2);
	close(fderr);*/

	pid_t f = fork();

	// int fdout = dup(1);

	if( f == 0)
	{
		printf("\nSou o filho: PID: %d\n",getpid());
		fflush(stdout);

		dup2(fdpass, 0);
		close(fdpass);

		dup2(fdsaida, 1);
		close(fdsaida);

		dup2(fderr, 2);
		close(fderr);
		
		while((res = read(0, buffer, 100)) > 0)
		{
			write(1, buffer, res);
			write(2, buffer, res);
		}
		_exit(0);
	}
	else
	{
		close(fdpass);
		close(fdsaida);
		close(fderr);

		printf("\nSou o pai: PID: %d\n",getpid());
		
		int status;
		wait(&status);

	}

	return 0;
}