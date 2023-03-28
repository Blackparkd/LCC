#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

// CHMOD CALCULATOR


int mycp(const char* from_path, const char* to_path){

	printf("from: %s to: %s\n",from_path,to_path );

	char buf[100];
	ssize_t res_read;
	ssize_t res_write;
	int nsyscalls =0;
	ssize_t bytes_read=0;
	ssize_t bytes_written=0;

	

	int fdo = open(from_path, O_RDONLY);
	if(fdo<0){
		perror("Erro open: ");      

		// perror dá a mensagem do último erro. pode ser de outra system call

		return -1;
	}

	int fdd = open(to_path, O_WRONLY | O_CREAT | O_TRUNC, 0640);


	while((res_read = read(fdo, buf, 100)) > 0){

		bytes_read+=res_read;
		printf("ret: %zu read %c\n", res_read, buf[0]);
		res_write = write(fdd, buf, res_read);
		bytes_written+=res_write;
		nsyscalls+=2;

	}
	printf("Bytes read: %zd\nBytes written:%zd\n",bytes_read,bytes_written);


	close(fdo);
	close(fdd);

	return 0;
}







int main(int argc, char* argv[]){

	mycp(argv[1],argv[2]);

	return 0;
}

// FEITO