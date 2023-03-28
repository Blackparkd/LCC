////// Pergunta 5 /////////

NEW_PERSON(char* Nome, int age){
	
	int res;

	Person p;
	p.age = age;
	strcpy(p.name, name);
	int fd = open(FILENAME,O_WRONLY | O_CREAT | O_APPEND,0600)

	res = write(fd,&p,sizeof(p));

	if (res<0){
		perror ("Error creating person");
		return -1;
	}

	close(fd);
}

CHANGE_AGE(char* name, int age){
	
	int res;

	Person P;
	
	int fd = open(FILENAME,O_RDWR, 0600);

	if(fd<0){
		perror("Error open");
		return -1;
	}

	while (read(fd, &p , sizeof(P)) > 0){

		printf("Read person name %s age - &d")

		if(strcmp(p.name,name) == 0){
			p.age = age;

			res = lseek(fd, ~sizeof(Person),SEEK_CUR);

			if (res<0)
		}
	}

}



int person_change_age_v2(long pos){

	Person p;

	int fd = open(FILENAME,O_RDWR,0600);

	
	if(fd<0){
		perror("Error open");
		return -1;
	}

	while (read(fd, &p , sizeof(P)) > 0){

		printf("Read person name %s age - &d")

		if(strcmp(p.name,name) == 0){
			p.age = age;

			res = lseek(fd, ~sizeof(Person),SEEK_CUR);

			if (res<0)
		}
}