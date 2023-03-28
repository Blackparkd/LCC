#include <unistd.h> /*chamadas ao sistema: defs e declarações essenciais*/
#include <stdio.h>

int main(int argc, char** argv)
{
	int ret;
	char *exec_args[] = {"/ls","-l",NULL};

	//argv[0] é o nome do programa
	// por isso passamos ao argv[0] o nome do programa
	// se lhe passarmos outra coisa, ele muda o nome do programa



	//ret = execl("/bin/ls","/bin/ls","-l",NULL);
	
	//ret = execlp("ls","/bin/ls","-l",NULL);

	//ret = execv("/bin/ls",exec_args);

	ret = execvp("ls",exec_args);

	printf("returned %d\n",ret); //se o exec for bem executado, não chega a este print

	perror("reached return");

	return 0; 


}