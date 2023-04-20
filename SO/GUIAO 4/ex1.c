#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>


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
		// PAI //

		close(p[0]);

		int i = 1000;
		
		printf("Pai escreveu %d\n",i);
		write(p[1],&i,sizeof(int));

		close(p[1]);

		wait(NULL);
	}

}



int main(int argc, char** argv)
{
	pai_to_filho();	
	//filho_to_pai();

	return 0;
}