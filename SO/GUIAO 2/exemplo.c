#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>



int main(int argc, char** argv){

	//printf("pid programa %d\n",getpid());
	//printf("pid pai %d\n",getppid());

	printf("a\n");


	int res = 0;
	int i = 0;

	res = fork();

	if(res==0){

		i++;
		
		printf("valor i %d\n",i);
		printf("Sou o filho %d\n",getpid());
		printf("Sou o filho - pid pai %d\n",getppid());
		sleep(10);
		_exit(10);


	} else {
		
		i--;
		
		wait(NULL);
		printf("valor i %d\n",i);
		printf("sou o pai %d\n",getpid());
		printf("sou o pai - pid pai %d\n",getppid());
		
	}

	printf("d\n");



	return 0;
}