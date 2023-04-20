#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>


int main(int argc, char **argv)
{
	int i;

	

	for(i = 0; i < 10; i++)
	{
		pid_t p = fork();
		
		if(p == 0)
		{
			printf("Filho: PID: %d  || PID do pai: %d\n", getpid(), getppid());
			_exit(i);
		}

		else
		{
			int status;
			pid_t pid = wait(&status);

			if(WIFEXITED(status))
			{
				printf("Pai: PID: %d || PID do filho: %d || Filho retornou: %d\n\n",getpid(),pid,WEXITSTATUS(status));
			}
		}
	}

	return 0;
}