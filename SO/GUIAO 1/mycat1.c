#include <stdio.h>
#include <unistd.h>


// END: CTRL+D


int main(int argc, char* argv[]){

	char buf[100];
	int write_bytes=0;
	int bytes_read=0;


	while((bytes_read = read(0,buf,100)) > 0 )
	{
		write_bytes+=write(1,buf,bytes_read);

		printf("\n\n%d\n\n\n", bytes_read);
	}

	if(write_bytes-bytes_read!=0)
		return -1;
	else
		return 0;
}