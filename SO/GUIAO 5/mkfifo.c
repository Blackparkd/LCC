#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>

//mostrar criação fifo e erro caso exista

int main(int argc, char *argv[])
{
	int res = mkfifo("fifo",0666);

	if(res == -1)
	{
		perror("erro abrir fifo");
		return -1;
	}

	return 0;
}