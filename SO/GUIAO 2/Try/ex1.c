#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

int main(int argc, char **argv)
{
	pid_t p = fork();

	if(p == 0)
	{
		printf("Filho: PID: %d  || PID do pai: %d\n", getpid(), getppid());
		_exit(0);

	}

	else
	{
		printf("Pai: PID: %d  || PID do pai: %d\n",getpid(),getppid());
	}
	return 0;
}