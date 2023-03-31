#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <time.h>

int main(int argc, char *argv[])
{
	int ret=0;
	char *argumentos[] = {"/ls","-l",NULL};

	pid_t p = fork();

	if(p == 0)
	{
		printf("Filho: PID %d\n\n", getpid());

		//ret = execl("/bin/ls","/bin/ls","-l",NULL);
		
		//ret = execlp("ls","/bin/ls","-l",NULL);
		
		//ret = execv("/bin/ls",argumentos);
		
		ret = execvp("ls",argumentos);

		perror("Exec deu erro\n");

		_exit(ret);
	}
	else
	{
		int status;
		int wait_ret = wait(&status);

		if(WIFEXITED(status))
		{
			printf("\nPai: PID: %d || Filho PID: %d\n",getpid(),wait_ret);
			printf("O filho retornou %d\n",WEXITSTATUS(status));
		}
		else
		{
			printf("O filho não terminou.\n");
		}
	}
	return ret;
}