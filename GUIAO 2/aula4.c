

int res = 0;
printf("a\n");

res = fork();

if (res==0){
	printf("b %d\n", res);
	_exit(-1);
}
else{
	printf("c %d\n", res);
	int status;
	//sleep(20);
	pid_t pid = wait(&status);
	if(WIFEXITED(status)){
		printf("filho %d retornou %d\n",pid,WEXITSTATUS)//incompleto)
	}
}

///// EX3 //////
int main(){

	int max = 10;
	for(int i =1;i<max;i++){
		pid_t p = fork();
		if (p == 0){
			printf("b %d\n", p);

			_exit(i);
		}
		else
		{
			printf("c %d\n, p");
			int status;
			pid_t pid = wait(&status);

			if(WIFEXITED(status)){
				printf("filho %d retornou %d\n",pid,WEXITSTATUS)
			}
		}

	}

	return 0;
}

///// EX4 //////

int max = 10;
for(int i = 1; i<=max; i++){
	pid_t p = Fork();

	if (p == 0){


		_exit(i);
	}
for(i=1; i<max; i++){
	int status;
	wait(&status);
}
}
