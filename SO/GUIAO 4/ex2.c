#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>

void filho_to_pai()
{
	int p[2];
	pipe(p);	

	if(fork() == 0)
	{
		close(p[0]);

		int i = 1000;
		
		printf("Filho vai escrever %d\n",i);

		write(p[1],&i,sizeof(int));

		close(p[1]);

		wait(NULL);
	}
	else
	{
		close(p[1]);

		int res;

		read(p[0],&res,sizeof(int));

		printf("Pai leu %d\n",res);

		close(p[0]);

		_exit(0);
	}
}


void pai_to_filho()
{

	int p[2];
	pipe(p);
	printf("p[0] %d, p[1] %d\n",p[0],p[1]);

	

	if(fork() == 0)
	{
		// FILHO //

		close(p[1]);

		int res;

		read(p[0],&res,sizeof(int));

		printf("Filho leu %d\n",res);

		close(p[0]);

		_exit(0);
	}
	else
	{
		// PAI // // se o pai faz wait antes de escrever, vai dar deadlock
				  // porque o filho nunca termina porque está a espera do read do pai

		close(p[0]);

		int i = 1000;
		
		printf("Pai vai escrever %d\n",i);

		sleep(10);

		write(p[1],&i,sizeof(int));

		close(p[1]);

		wait(NULL);
	}

}



int main(int argc, char** argv)
{
	//pai_to_filho();	
	filho_to_pai();

	return 0;
}