#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <limits.h>

int main(int argc, char *argv[])
{
	int res;
	char buffer[100];

	int p[2];
	pipe(p);

	pid_t f = fork();

	if(f == 0)
	{
		close(p[1]);

		printf("\nSou o filho: PID: %d\n",getpid());
		fflush(stdout);

		dup2(p[0],0);
		
		close(p[0]);
		
		execlp("wc","wc",NULL);

		_exit(0);
	}
	else
	{
		close(p[0]);

		while((res = read(0, buffer, 100)) > 0)
		{
			write(p[1], buffer, res);			
		}

		close(p[1]); // fechar descritor antes de wait
					 // senão dá deadlock

		int status;
		wait(&status);
	}



	return 0;
}