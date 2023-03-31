#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <time.h>

int main(int argc, char** argv)
{
	int fork_ret = fork();
	int status;
	int exec_args;

	if (fork_ret == 0)
	{
		printf("Sou o filho %d\n",getpid());

		int exec_ret = execv("/bin/lsdasdas",exec_args);
		perror("reached return");

		_exit(exec_ret);
	}
	else
	{
		printf("Sou o pai %d\n",getpid());
		int wait_ret = wait(&status);

		if(WIFEXITED(status))
		{
			printf("O filho retornou %d\n",WEXITSTATUS(status));
		}
		else
		{
			printf("O filho não terminou.\n");
		}
	}
}