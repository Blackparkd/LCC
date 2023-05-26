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

int exec_command(char* arg){
	char* exec_args[10];

	char* string;
	int exec_ret = 0;
	int i = 0;

	char*  command = strdup(arg);

	while((string = strsep(&command," "))!= NULL){
		exec_args[i] = string;
		i++;
	}
	exec_args[i] = NULL;

	exec_ret = execvp(exec_args[0],exec_args);

	return exec_ret;
}

int main(int argc, char *argv[])
{
	char * commands[] = {
		"grep -v ^# /etc/passwd",
		"cut -f7 -d:",
		"uniq",
		"wc -l"
	};
	
	int nrc = 4;
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

				exec_command(commands[i]);
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

				exec_command(commands[i]);
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

				exec_command(commands[i]);
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