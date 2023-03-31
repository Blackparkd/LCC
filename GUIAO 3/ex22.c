#include <stdio.h>
#include <sys/wait.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
	char *execArgs[] = {"ls","-l",NULL};
	int status;
	

	pid_t p = fork();

	if(p == 0)
	{
		printf("Sou o filho.\n");
		
		int retu = execvp("ls",execArgs);
		
		perror("Exec correu mal");
		
		_exit(retu);
	}
	else
	{	
		wait(&status);

		if(WIFEXITED(status))
		{
			printf("Sou o pai: O filho %d retornou %d.\n",getpid(),WEXITSTATUS(status));
		}
	}
	return 0;
}