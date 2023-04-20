#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <limits.h>

int main(int argc, char *argv[])
{
	int read_bytes;

	char buffer[4096];

	printf("%d\n", PIPE_BUF);

	int fd = open("fifo",O_RDONLY);

	printf("depois open readfifo\n");

	while((read_bytes = read(fd, buffer, 4096)) > 0)
		write(1, buffer, read_bytes);

	// descritor 1 é standard output, 2 é para standard error, 0 para standard input

	printf("depois while readfifo\n");

	close(fd);

	unlink("fifo");

	return 0;
}