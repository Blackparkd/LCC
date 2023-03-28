#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <time.h>


//   ########## EX 5 ############

int main(int argc, char** argv)
{
	if (argc < 2)
	{
		printf("Usage: program <needle>\n");
		exit(-1);
	}

	//pid_t pid;
	int needle = atoi(argv[1]);
	int rows = 100; //linhas
	int cols = 1000; //colunas
	int rand_max = 1000;
	//int status;
	int **matrix;

	//seed random numbers
	
	srand(time(NULL));

	//alocar e encher matriz com numeros random

	printf("Generating numbers from 0 to %d...\n",rand_max);
	matrix = (int**) malloc(sizeof(int*) *rows);
	for(int i = 0; i < rows; i++)
	{
		matrix[i] = (int*) malloc(sizeof(int) *cols);
		for(int j = 0; j < cols; j++)
		{
			matrix[i][j] = rand() % rand_max;
		}
	}
	printf("Done.\n\n");




	// Abrir os filhos para cada linha

	for (int i = 0; i < rows; i++)
	{
		pid_t p = fork();

		if (p == 0)
		{

			for (int j = 0; j < cols; j++)
			{
				//printf("Sou o filho: %d\nPID: %d\n\n",i,p);
				if ( needle == matrix[i][j] )
				{
					printf("Sou o filho %d e encontrei o número %d em (%d,%d).\n\n",i,needle,i,j);
					sleep(1);
				}
				/*else
				{
					printf("Vou procurar mais.\n\n");
				}
				*/
			}
			
			printf("Filho %d a acabar.\n\n",i);
			_exit(i);
		}
	}



	for (int i = 0; i < rows; i++)
	{
		int status;
		pid_t pid = wait(&status);

		if(WIFEXITED(status))
		{
			printf("Sou o pai:  Filho %d retornou %d\n\n",pid,WEXITSTATUS(status));
		}
		else
		{
			printf("filho foi interrompido\n\n");
		}
	}

	return 0;
}

