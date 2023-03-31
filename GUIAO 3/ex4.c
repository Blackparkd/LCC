#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <string.h>
#include <stdlib.h>

int mysystem(){

	char *Args[20];

	string = strsep(&Args," ");

	int i = 0;
	char *exec_args;

	while(string != NULL)
	{
		exec_args[i] = string;
		string = strsep(&command," ");
		i++;
	}

	exec_args[i] = NULL;

	
	*Args = strsep(argv," ");

	for(int i = 1; i < argc; i++)
	{
		pid_t p = fork();
		
		if (p == 0)
		{
			printf("Sou o filho %d, PID: %d\n",i,p);

			int exec_ret = execlp(argv[i],argv[i],NULL);

			_exit(exec_ret);
		}
		else
		{
			printf("Filho nº %d: %d\n",i,p);
		}
		printf("\n");
	}
}


int main(int argc, char** argv)
{


	execvp(argv[0],Args);

	
	char comando1[] = "ls -l -a -h";
	char comando2[] = "sleep 10";
	char comando3[] = "ps";	
	int ret;
 
	printf("a executar mysystem para %s\n",comando1);
	ret = mysystem(comando1);
	printf("ret: %d\n",ret);
 
	printf("a executar mysystem para %s\n",comando2);
    mysystem(comando2);
 
	printf("a executar mysystem para %s\n",comando3);
	mysystem(comando3);
 
 
	return 0;

}
