//pai lê de um ficheiro e escreve as linhas para o descritor de escrita
// pai tem de criar um pipe para cada filho e escrever uma linha em cada

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <time.h>


#define RANDOM_MAX (int)10e6


ypedef struct Minfo{
	int line_nr;
	int ocur_nr;
}Minfo;

int main(int argc, char** argv)
{
	int i,j=0;

	int nLinhas = strstr()

	for(i = 0; i < nLinhas; i++)
	{
		pid_t f = fork();
		
		int p[2];

		pipe(p);
		
		if(f == 0)
		{
			// Fechar o descritor de escrita, filho não vai escrever
			close(p[1]);

			while((read_res = read(p[0],&res,sizeof(Minfo))) > 0)
			{
				continue; //tbd
			}
			
			close(p[0]);
			
			_exit(i);
		}
	}

	// nao é preciso fechar o descritor de leitura nem de escrita, pai vai ler
	// do ficheiro e escrever para outro

	//	close(p[0]);
	
	ssize_t read_res;

	// Pai vai parar de ler quando receber End Of File (EOF)
	// que acontece quando todos os descritores de escrita estão todos fechados
	
	close(p[1]);

	

	for(i = 0; i < nLinhas; i++)
	{
		Minfo res;
		
		write(p[1],&res,sizeof(Minfo));

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
	
	return 0;
}