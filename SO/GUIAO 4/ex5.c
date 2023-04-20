#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>

#define MAX 100000



void filho_to_pai()
{
	int p[2];
	pipe(p);	

	if(fork() == 0)
	{
		// FILHO //

		close(p[0]);

		int i;
		

		for(i=1; i<MAX; i++)
		{
			printf("Filho vai escrever %d\n",i);

			write(p[1],&i,sizeof(int));
		}

		printf("Filho saiu do ciclo\n");

		close(p[1]);

		_exit(0);
	}

	else
	{
		// PAI //

		close(p[1]); // se nao se fechar logo o extremo de escrita, o read
					 // do pai fica à espera dele mesmo, porque enquanto o 
					 // de escrita ta aberto, o de leitura nao fecha, e bloqueia

		int res[2];

		ssize_t read_res;

		while((read_res = read(p[0],res,sizeof(int)*2)) > 0) // sizeof(int)*2 para ler dois inteiros em vez de 1
		{
			if(read_res == 8)
			{
				printf("Pai leu res[0] %d res[1] %d e retornou %lu\n",res[0],res[1],read_res);	
			}
			else
			{
				printf("Pai leu res[0] %d e retornou %lu\n",res[0],read_res);
			}
			
		}

		printf("Pai saiu do while\n");

		wait(NULL);

		close(p[0]); // se fizermos close() antes do wait(), acontece um sigpipe,
					 // o s.o. mata o escritor
	}
		
}


int main(int argc, char** argv)
{
	//pai_to_filho();	
	filho_to_pai();

	return 0;
}