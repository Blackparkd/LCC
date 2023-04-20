#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>


int main(int argc, char *argv[])
{
	int read_bytes;

	char buffer[4096];

	int fd = open("fifo",O_WRONLY);

	printf("depois open writefifo\n");

	while((read_bytes = read(0,buffer,4096))>0)
	{
		write(fd, buffer, read_bytes);
	}

	printf("depois while writefifo\n");

	sleep(4);
	
	close(fd);

	return 0;
}