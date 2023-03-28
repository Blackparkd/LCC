#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>


//  #########    EX4    ########### 


int main(int argc, char* argv[])
{
	int max = 10;
	for(int i = 1; i<=max; i++)
	{
		pid_t p = fork();
		if (p == 0)
		{
			printf("Sou o filho %d, PID: %d\n",i,p);
			_exit(i);
		}
		putchar('\n');
		/*else
		{
			printf("Filho nº %d: %d\n",i,p);
		}
		printf("\n");
		*/
	}
	
	for(int i = 1; i<=max; i++)
	{
		int status;
		pid_t pid = wait(&status);

		if(WIFEXITED(status))
		{
			printf("Filho %d retornou %d\n",pid,WEXITSTATUS(status));
		}
		else
		{
			printf("Filho foi interrompido\n");
		}

		putchar('\n');

	}
	return 0;
}


///// DONE ////// 