#include <stdio.h>
#include <sys/wait.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
	char *execArgs[] = {"ls","-l",NULL};

	for(int i = 1; i < argc; i++)
	{
		pid_t p = fork();

		if(p == 0)
		{
			printf("Sou o filho.\n");
		
			int retu = execlp(argv[i],argv[i],NULL);
		
			perror("Exec correu mal");
		
			_exit(retu);
		}
	}

	for(int i = 1; i < argc; i++)
	{
		int status;
		
		wait(&status);

		if(WIFEXITED(status))
		{
			printf("Sou o pai: O filho %d retornou %d.\n",getpid(),WEXITSTATUS(status));
		}
		else
		{
			perror("Filho interrompido");
		}
	}
	printf("Os filhos terminaram todos.\n");
	return 0;
}
	

	