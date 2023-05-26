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
	int p[2];
	pipe(p);

	pid_t f = fork();

	if(f == 0)
	{
		close(p[0]);

		dup2(p[1],1); // substitui o 1 pelo p[1] // 
		
		close(p[1]);
		
		execlp("ls","ls","/etc",NULL);

		_exit(0);
	}

	pid_t fo = fork();

	if (fo == 0)
	{
		close(p[1]);

		dup2(p[0],0);

		close(p[0]);

		execlp("wc","wc","-l",NULL);

		_exit(0);
	}

	int status;
	wait(&status);

	// POR O PAI A ESPERAR PELOS FILHOS

	return 0;
}