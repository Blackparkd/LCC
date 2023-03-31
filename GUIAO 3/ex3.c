#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>

int main(int argc, char* argv[])
{
	for(int i = 1; i < argc; i++)
	{
		pid_t p = fork();
		
		if (p == 0)
		{
			printf("Sou o filho %d, PID: %d\n",i,p);

			int exec_ret = execlp(argv[i],argv[i],NULL);

			_exit(exec_ret);
		}
		else
		{
			printf("Filho nº %d: %d\n",i,p);
		}
		printf("\n");
	}
	
	for(int i = 1; i < argc; i++)
	{
		int status;

		int wait_ret = wait(&status);

		if(WIFEXITED(status))
		{
			printf("Filho %d retornou %d\n",getpid(),WEXITSTATUS(status));
		}
		else
		{
			printf("Filho foi interrompido\n");
		}
	printf("Os filhos terminaram todos.\n");
	}
	return 0;
}
