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
	
	char * commands[] = {
		"ls",
		"cat",
		"wc"
	};

	int nrc = 3;
	

	int pipes[nrc-1][2];

	for(int i = 0; i < nrc ; i++)
	{
		if (i == 0)
		
		{
			pipe(pipes[i]);

			if(fork() == 0)
			{
				close(pipes[i][0]);
				dup2(pipes[i][1],1);
				close(pipes[i][1]);

				execlp(commands[i],commands[i],NULL);
			}
			else
			{
				close(pipes[i][1]);
			}

		}

		else if (i == nrc - 1)
		
		{
			if(fork() == 0)
			{
				dup2(pipes[i-1][0],0);
				close(pipes[i-1][0]);

				execlp(commands[i], commands[i], NULL);
			}
			else
			{
				close(pipes[i-1][0]);
			}

		}

		else
		
		{
			pipe(pipes[i]);

			if(fork() == 0)
			{
				close(pipes[i][0]);
				dup2(pipes[i-1][0],0);
				close(pipes[i-1][0]);
				dup2(pipes[i][1],1);
				close(pipes[i][1]);

				execlp(commands[i], commands[i], NULL);
			}
			else
			{
				close(pipes[i-1][0]);
				close(pipes[i][1]);
			}

		}

	}

	for( int i = 0; i < nrc; i++)
	{
		int status;

		wait(&status);

	}

	return 0;
}