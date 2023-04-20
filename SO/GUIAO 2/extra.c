

for(i = 0; i<rows; i++)
{
	if(fork()==0)
	{
		int fd = open();
		lseek(fd, i*cols*sizeof(int), seek_set);
		read();
	}	
}
