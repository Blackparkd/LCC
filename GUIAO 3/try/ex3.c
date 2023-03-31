#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <time.h>

int main(int argc, char* argv[])
{
	char *execArgs[] = {"ls","-l",NULL};
	
	int i;

	for(i=1; i < argc; i++)
	{
		pid_t p = fork();

		if (p == 0)
		{
			printf("Filho: PID: %d\n",getpid());
			
			int	ret = execlp(argv[i],argv[i],NULL);
			
			perror("Exec deu erro");

			_exit(ret);
		}
	}

	for(i=1; i < argc; i++)
	{
		int status;
		int wait_ret = wait(&status);

		if(WIFEXITED(status))
		{
			printf("\nPai: PID: %d || PID do filho: %d || Filho retornou: %d\n",getpid(), wait_ret, WEXITSTATUS(status));
		}
		else
		{
			perror("Filho interrompido");
		}
	}
	printf("\n\nOs filhos terminaram todos\n");
	return 0;
}