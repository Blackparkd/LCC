#include "tracer.h"

void runCommand(char* command[]){				// FUNÇÃO PARA O EXECUTE -U
	
	char buffer[1024];
	
	// Medir o tempo inicial //

	struct timeval current_time;
  	gettimeofday(&current_time, NULL);
	int start = (current_time.tv_sec)*1000000 + (current_time.tv_usec);

	int fdMonitor = open("args_fifo",O_WRONLY);

	pid_t f = fork();

	if(f < 0)
		perror("Fork error");

	else if (f == 0){
		int nbytesOut;
		int nbytesServer;
		
		pid_t pid = getpid();

		// Passar infos ao monitor //

		ssize_t byte = sizeof(pid) + sizeof(command[0]) + sizeof(start);

		nbytesServer = sprintf(buffer,"UI%05lu%d %s %d\n",byte+2, pid, command[0], start);

		write(fdMonitor, buffer, nbytesServer);

		nbytesOut = sprintf(buffer,"\nRunning PID %d\n\n", pid);
		
		// Passar PID ao utilizador //
		write(1, buffer, nbytesOut);

		execvp(command[0],command); 

		write(2,"Exec correu mal",15);

		_exit(0);
		
	}else{

		int status;
		wait(&status);

		if(WIFEXITED(status)){
			gettimeofday(&current_time, NULL);
			int finish = (current_time.tv_sec)*1000000 + (current_time.tv_usec);
			int tempo = (finish - start)/1000 + (finish - start)%1000;
			//int nbytesServer;
			//ssize_t byte = sizeof(f) + sizeof(start);

			/*nbytesServer = sprintf(buffer,"UF %05lu%d %d\n",byte+2, f, finish);
			write(fdMonitor, buffer, nbytesServer); */

			int read_bytes = sprintf(buffer,"\nEnded in %d ms\n",tempo);

			write(1, buffer, read_bytes);

			putchar('\n');
		}
		else 
			perror("Program interrupted");
	}

	// Fechar descritores //

	close(fdMonitor);
}

int exec_command(char* arg){ 			// AUXILIAR PARA A PIPELINE
	char* exec_args[20];

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

int runPipe(int nrc, char* commands[]){			//FUNÇÃO PIPELINE

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

		if(WIFEXITED(status)){
			dup(0); dup(1);
		}
		else
			perror("Program interrupted");
	}
	return 0;
}



int runStatus(pid_t pid){  // FUNÇÃO STATUS

	int fdMonI = mkfifo("fif_mon",0666);

	if(fdMonI == -1){
		perror("Fifo unopened");
		return -1;
	}

	unlink("fif_mon");
	return 0;
}

/*
############################################################################

							// MAIN //

############################################################################
*/

int main(int argc, char *argv[]){
	char buffer[1024];

	if (argc == 1){
		int nbytes = sprintf(buffer,"Error: Command unknown\n");
		write(2, buffer, nbytes);
		return -1;
	}

	if(!strcmp(argv[1],"execute")){
		
		if(!strcmp(argv[2],"-u")) // CORRE O EXECUTE -U 
			runCommand(&argv[3]);
		
		
		else if(!strcmp(argv[2],"-p")){	// CORRE O EXECUTE -P (PIPELINE)

			char* exec_args[15];
			char* string;
			int i = 0, nArgs = 0;
			char**  command = &argv[3];

			while((string = strsep(command,"|"))!= NULL){
				exec_args[i] = string;
				i++; nArgs++;
			}

			exec_args[i] = NULL;			

			char * commands[] = {
				"grep -v ^# /etc/passwd",
				"cut -f7 -d:",
				"uniq",
				"wc -l"
			};

			runPipe(4, commands);
		}
	}


	else if(strcmp(argv[1],"status") == 0){ // CORRE O STATUS
		//pid_t pid = getpid();
		//printf("sadas");
		//runStatus(pid);
	
	}else{

		printf("Comando desconhecido\n");
	}

	return 0;
}

