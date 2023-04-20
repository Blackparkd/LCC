#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <time.h>

int main(int argc, char** argv)
{
	if (argc < 2)
	{
		printf("Usage: program <needle>\n");
		exit(-1);
	}

	//pid_t pid;
	int needle = atoi(argv[1]);
	int rows = 10; //linhas
	int cols = 10000; //colunas
	int rand_max = 10000;
	//int status;
	int **matrix;

	//seed random numbers
	
	srand(time(NULL));

	//alocar e encher matriz com numeros random

	printf("Generating numbers from 0 to %d...\n",rand_max);
	matrix = (int**) malloc(sizeof(int*) *rows);
	for(int i = 1; i <= rows; i++)
	{
		matrix[i] = (int*) malloc(sizeof(int) *cols);
		for(int j = 1; j <= cols; j++)
		{
			matrix[i][j] = rand() % rand_max;
		}
	}
	printf("Done.\n\n");

	int i,j=0;
	int encontrou = 0;

	for(i = 1; i <= rows; i++)
	{
		pid_t p = fork();
		
		if(p == 0)
		{
			for(j = 1; j <= cols && encontrou == 0; j++)
			{
				if(needle == matrix[i][j])
				{
					printf("Filho %d: PID: %d  || Encontrei o número %d em (%d,%d)\n", i, getpid(), needle, i, j);
					encontrou = 1;	
					_exit(i);
				}	
			}
			printf("Filho %d: PID: %d  || Não encontrei\n",i,getpid());
			_exit(0);
		}
	}

	for(i = 1; i <= rows; i++)
	{
		int status;
		pid_t pid = wait(&status);

		if(WIFEXITED(status))
		{	
			printf("Pai: PID: %d || PID do filho: %d || Filho retornou: %d\n",getpid(),pid,WEXITSTATUS(status));
		}

	}
	
	return 0;
	
} // Falta terminar todos os processos mal encontre uma vez o número