#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

int main(int argc, char **argv)
{
	pid_t p = fork();
	int status;

	if(p == 0)
	{
		printf("Filho: PID: %d  || PID do pai: %d\n", getpid(), getppid());
		_exit(0);

	}

	else
	{
		pid_t pid = wait(&status);
		printf("Pai: PID: %d  || PID do pai: %d  || PID do filho: %d\n",getpid(),getppid(),pid);
	}
	return 0;
}