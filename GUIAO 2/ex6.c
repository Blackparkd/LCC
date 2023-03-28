#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <time.h>




int main(int argc, char** argv[])
{
	if (argc < 2)
	{
		printf("Usage: program <needle>\n");
		exit(-1);
	}

	//pid_t pid;
	int needle = atoi(argv[1]);
	int rows = 100; //linhas
	int cols = 200; //colunas
	int rand_max = 100;
	int status;
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
					_exit(i);
				}
				_exit(-1);
			}
		}
	}

	int matches[100] = { 0 };

	for (int i = 0; i < rows; i++) {
        int pid = wait(&status);
        int linha = WEXITSTATUS(status);
        if (linha != 255)
            matches[linha] = 1;
    }

    printf("\n\n");

    for (int i = 0; i < rows; i++)
    {
        if (matches[i])
        {
            printf("Encontrado na linha %d\n", i);
        }
    }
	return 0;
}



















/*for(i=0; i < nrow; i++)
{
	pid_t p = fork();
	if (p == 0)
	{

	}
	else
	{
		ARR[i] = p;
		waitpid(1º);
	}

}
for(i=0; i<nrow; i++)
	{	
		waitpid(ARR[i],&status,0);
	}*/
