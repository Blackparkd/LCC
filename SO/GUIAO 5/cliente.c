#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <limits.h>
#include <string.h>

int main(int argc, char *argv[])
{
	int fd = open("fifo",O_WRONLY);

	write(fd, argv[1], strlen(argv[1]));

	close(fd);

	return 0;
}