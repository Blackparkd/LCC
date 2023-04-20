#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <time.h>
#include <fcntl.h>
#include <sys/types.h>
#include <limits.h>



void runCommand(char * command)
{
	
	char buffer[1024];


	// Medir o tempo inicial //

	struct timeval current_time;

  	gettimeofday(&current_time, NULL);

	int start = current_time.tv_sec;

	//printf("seconds : %ld\nmicro seconds : %ld\n", current_time.tv_sec, current_time.tv_usec);
	
	//  // 

	int fdMonitor = open("fifo",O_WRONLY);

	pid_t f = fork();

	if (f == 0)
	{
		int nbytesOut;
		int nbytesServer;


		gettimeofday(&current_time, NULL);
		
		int start = current_time.tv_sec;

		pid_t pid = getpid();

		// Passar PID ao monitor //
		nbytesServer = sprintf(buffer,"PID do programa a executar: %d\nNome do programa: %s\nTempo de início: %d segundos\n\n",pid, command, start); 

		write(fdMonitor, buffer, nbytesServer);

		nbytesOut = sprintf(buffer,"Pid do programa a executar: %d\n\n", pid);

		write(1, buffer, nbytesOut); // Passar PID ao utilizador

		sleep(1);

		execlp(command,command,NULL);
		
		printf("Exec correu mal\n");

		_exit(0);
	}

	int status;
	
	wait(&status); //int wait_ret = wait(&status);

	if(WIFEXITED(status))
	{
		printf("\nFilho bem executado, retornou %d\n",WEXITSTATUS(status));
		int finish = current_time.tv_sec;

		putchar('\n');
	}
	else
	{
		printf("\nFilho interrompido\n");
		putchar('\n');
	}


	// Medir o tempo final //

	gettimeofday(&current_time, NULL);

	int finish = current_time.tv_sec;

	printf("seconds : %ld\nmicro seconds : %ld\n", current_time.tv_sec, current_time.tv_usec);

	putchar('\n');

	int time = finish - start;

	printf("Tempo decorrido: %d segundos\n",time);

	//  //


	// Fechar descritores //

	close(fdMonitor);
}



// MAIN //

int main(int argc, char *argv[])
{
	char buffer[1024];

	if (argc == 1)
	{
		int nbytes = sprintf(buffer,"Erro. Insira um comando válido\n");
		write(1, buffer, nbytes);

		return -1;
	}
	

	if(strcmp(argv[1],"execute") == 0)
	{
		runCommand(argv[2]);
	}

	else if(strcmp(argv[1],"status") == 0)
	{
		//runStatus();

	}
	else
	{
		printf("Comando desconhecido\n");
	}

	return 0;
}

