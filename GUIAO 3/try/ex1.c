#include <unistd.h> /*chamadas ao sistema: defs e declarações essenciais*/
#include <stdio.h>

int main(int argc, char *argv[])
{
	int ret;
	char *argumentos[] = {"/ls","-l",NULL};

	ret = execl("/bin/ls","/bin/ls","-l",NULL);
	//ret = execlp("ls","/bin/ls","-l",NULL);
	//ret = execv("/bin/ls",argumentos);
	//ret = execvp("ls",argumentos);

	return ret;
}