#include <stdio.h>
#include <sys/wait.h>
#include <unistd.h>

int main(int argc, char **argv)
{
	int retu;

	char *execArgs[] = {"ls","-l",NULL};

	//retu = execl("/bin/ls","/bin/ls","-l",NULL);

	//retu = execlp("ls","/bin/ls","-l",NULL);

	//retu = execv("/bin/ls",execArgs);

	retu = execvp("ls",execArgs);

	printf("Returned: %d.\n",retu);
	perror("Error: Reached return");
	return 0;
}