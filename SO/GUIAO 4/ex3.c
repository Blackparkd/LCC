#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>

void filho_to_pai()
{
	int p[2];
	pipe(p);	

	if(fork() == 0)
	{
		// FILHO //

		close(p[0]);

		int i = 1000;
		
		printf("Filho vai escrever %d\n",i);

		write(p[1],&i,sizeof(int));

		printf("Filho escreveu %d\n",i);


		close(p[1]);

		_exit(0);
	}

	else
	{
		// PAI //

		close(p[1]);

		int res;

		sleep(10);

		read(p[0],&res,sizeof(int));

		printf("Pai leu %d\n",res);

		close(p[0]);
		
		wait(NULL);
	}
		
}


int main(int argc, char** argv)
{
	//pai_to_filho();	
	filho_to_pai();

	return 0;
}

#define RANDOM_MAX (int)10e6


typedef struct Minfo{
	int line_nr;
	int ocur_nr;
}Minfo;
