#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

int main(int argc, char **argv)
{
	int i;

	for(i = 1; i <= 10; i++)
	{
		pid_t p = fork();
		
		if(p == 0)
		{
			printf("Filho: PID: %d  || PID do pai: %d\n", getpid(), getppid());
			_exit(i);
		}
		
	}

	for(i = 1; i <= 10; i++)
	{
		int status;
		pid_t pid = wait(&status);

		if(WIFEXITED(status))
		{
			printf("Pai: PID: %d || PID do filho: %d || Filho retornou: %d\n",getpid(),pid,WEXITSTATUS(status));
		}

	}
	return 0;
}