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
		"grep -v ^# /etc/passwd",
		"cut -f7 -d:",
		"uniq",
		"wc -l"
	};
	
	int nrc = 4;

	char* cmd; char* token;
	char* cmds[4];
	
	for(int j = 0; j < 4; j++){
		
		cmd = strdup(commands[j]);
		int i=0;

		while((token = strsep(&cmd, " "))!= NULL){
			printf("%s\n", token);
			cmds[i++] = token;
		}
		cmds[i] = NULL;

		int pipes[nrc-1][2];

		for(j = 0; cmds[j] != NULL; i++){
			if (i == 0){
				pipe(pipes[i]);
				
				if(fork() == 0){
					close(pipes[i][0]);
					dup2(pipes[i][1],1);
					close(pipes[i][1]);

					execvp(cmds[i],cmds);
				}
				else
					close(pipes[i][1]);
			}

			else if (i == nrc - 1){
				if(fork() == 0){
					dup2(pipes[i-1][0],0);
					close(pipes[i-1][0]);

					execvp(cmds[i], cmds);
				}
				else
					close(pipes[i-1][0]);
			}
			else{
				pipe(pipes[i]);

				if(fork() == 0){
					close(pipes[i][0]);
					dup2(pipes[i-1][0],0);
					close(pipes[i-1][0]);
					dup2(pipes[i][1],1);
					close(pipes[i][1]);

					execvp(cmds[i], cmds);
				}
				else{
					close(pipes[i-1][0]);
					close(pipes[i][1]);
				}
			}
		}

		for( int i = 0; i < nrc; i++){
			int status;

			wait(&status);
		}
		return 0;
	}
}