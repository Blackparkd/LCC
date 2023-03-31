#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>

#define MAX 10000

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

		close(p[1]);

		int res;

		read(p[0],&res,sizeof(int));

		printf("Pai leu %d\n",res);

		wait(NULL);

		close(p[0]);
	}
		
}


int main(int argc, char** argv)
{
	//pai_to_filho();	
	filho_to_pai();

	return 0;
}