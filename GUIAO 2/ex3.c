#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

int main(int argc, char* argv[])
{

	int max = 10;
	for(int i =1;i<max;i++)
	{
		pid_t p = fork();
		if (p == 0)
		{
			printf("Sou o filho: %d\n pid: %d\n\nPai:%d\n\n", i,getpid(),getppid());

			_exit(i);
		}
		else
		{
			int status;
			pid_t pid = wait(&status);

			
			if(WIFEXITED(status))
			{
				printf("Sou o pai:\nFilho %d retornou %d\n\n",pid,WEXITSTATUS(status));
				
			}	
			else
			{
				printf("filho foi interrompido\n");
			}
		}

	}

	return 0;
}