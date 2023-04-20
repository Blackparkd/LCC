#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <time.h>


#define RANDOM_MAX (int)10e6


typedef struct Minfo{
	int line_nr;
	int ocur_nr;
}Minfo;


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
	int result[rows];

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

	int p[2];

	pipe(p);

	for(i = 1; i <= rows; i++)
	{
		pid_t f = fork();
		
		if(f == 0)
		{
			// Fechar o descritor de leitura, filho não vai ler
			close(p[0]);

			Minfo x;

			x.line_nr = i;
			x.ocur_nr = 0;			

			for(j = 1; j <= cols; j++)
			{
				if(needle == matrix[i][j])
				{
					x.ocur_nr++;
					printf("Filho %d: PID: %d  || Encontrei o número %d em (%d,%d)\n", i, getpid(), needle, i, j);
				}	
			}
			write(p[1],&x,sizeof(Minfo));

			close(p[1]);
			
			_exit(i);
		}
	}

	// fechar o descritor de escrita, pai não vai escrever
	close(p[1]);
	
	Minfo res;
	ssize_t read_res;

	// Pai vai parar de ler quando receber End Of File (EOF)
	// que acontece quando todos os descritores de escrita estão todos fechados
	
	while((read_res = read(p[0],&res,sizeof(Minfo))) > 0)
		{
			result[res.line_nr] = res.ocur_nr;
		}
	close(p[0]);


	for(i = 1; i <= rows; i++)
	{
		
		int status;
		pid_t pid = wait(&status);

		if(WIFEXITED(status))
		{	
			printf("Pai: PID: %d || PID do filho: %d || Filho retornou: %d\n",getpid(),pid,WEXITSTATUS(status));
		}
		else
		{
			printf("Error\n");
		}

	}
	
	putchar('\n');
	// Impressão do array

	for(i = 1; i <= rows; i++)
	{
		if(!result[i])
		{
			printf("\nNão encontrado na linha %d\n",i);
		}
		else
		{
			printf("\nEncontrado na linha: %d: %d vezes\n",i ,result[i]);
		}
	}

	return 0;
}