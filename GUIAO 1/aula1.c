#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>

int mycp(const char* from_path, const char* to_path){




	char buf[100];
	ssize_t res_read;
	ssize_t res_write;

	int syscalls = 0;

	ssize_t bytes_read = 0;
	ssize_t bytes_written = 0;

	int fdo = open(from_path, O_RDONLY);
	if (fdo<0){
		perror("Erro open: ");
		return -1;
	}

	int fdd = open(to_path, O_WRONLY | O_CREAT | O_TRUNC, 0640);

	while((res_read = read(fdo,buf,100)) > 0){
		bytes_read += res_read;
		printf("ret: %zu read %c\n", res_read, buf[0]);
		res_write = write(fdd, buf, res_read);
		bytes_written += res_write;
		syscalls += 2;
	}
	printf("Bytes read: %zd\nBytes written: %zd\nSystem calls: %d\n",bytes_read,bytes_written,syscalls);
	
	close(fdo);
	close(fdd);
	return 0;	
}


/*int mycat(const char* from_path, const char* to_path){

	char buf[100];
	int write_bytes = 0;
	int bytes_read = 0;

	bytes_read = write(0,buf,bytes_read);

	while(bytes_read > 0){
		write_bytes += write(1,buf,bytes_read);
		printf("\n\n %d \n\n",bytes_read);
	}
	if(write_bytes != bytes_read){
		return -1;
	else
		return 0;
	}
}
*/




int main(int argc, char* argv[]){

	///////  MYCP  ///////
	//mycp(argv[1],argv[2]);

	///////  MYCAT ///////

	//mycat(argv[1],agrv[2]);

	/*char buf[100];
	int write_bytes = 0;
	int bytes_read = 0;

	bytes_read = write(0,buf,bytes_read);

	while(bytes_read > 0){
		write_bytes += write(1,buf,bytes_read);
		printf("\n\n %d \n\n",bytes_read);
	}

	if(write_bytes - bytes_read != 0)
		return -1;
	
	else
		return 0;
	

	return 0;
	*/

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