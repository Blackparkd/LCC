#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <string.h>
#include <stdlib.h>

int mysystem(char* command){

	int fork_ret, exec_ret, status, res;
	
	char *exec_args[20];
	char *string = NULL;
	char *cmd = strdup(command);

	string = strsep(&cmd," ");

	int i = 0;
	char *exec_args;

	//while((string = strsep(&cmd," "))!NULL)
	while(string != NULL)
	{
		exec_args[i] = string;
		i++;
	}

	exec_args[i] = NULL;

	fork_ret = fork();

	if (fork_ret == 0)
	{
		printf("Sou o filho\n");

		exec_ret = execvp(exec_args[0],exec_args);

		_exit(exec_ret);
	}
	else
	{
		//ver se o exec_ret retorna -1, para ver se o exec correu bem

		wait(&status);
		if(WIFEXITED(status))
		{
			printf("Sou o pai: Filho %d retornou:%d\n",getpid(),WEXITSTATUS(status));
		}
	}

}



int main(int argc, char** argv)
{


	execvp(argv[0],args);

	
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
