#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <limits.h>

int main(int argc, char *argv[])
{
	int res = mkfifo("fifo", 0666);

	if(res == -1)
	{
		perror("erro abrir fifo");
		return -1;
	}

	int read_bytes;

	char buffer[4096];

	while(1) // servidor nunca fecha
	{
		int fd = open("fifo",O_RDONLY);

		while((read_bytes = read(fd, buffer, 4096)) > 0)
		{
			write(1, buffer, read_bytes);
		}

		close(fd);
	}

	unlink("fifo");

	return 0;
}