#include "monitor.h"

#define SIZE 1024

void signal_handler_C(int signum){
    //unlink("tracer_to_monitor");
    unlink("args_fifo");
    unlink("to_tracer");
    printf("\n\nServidor terminou!\n");
    exit(0);
}

void addProcessoExecuted(Process* processo, char *program, char *pid, char *time_s){
	Process novoProcesso = malloc(sizeof(struct process));
    novoProcesso->pid = strdup(pid);
    novoProcesso->program = strdup(program);
    novoProcesso->time_s = strdup(time_s);
    novoProcesso->time_f = NULL;
    novoProcesso->next = *processo; 
    *processo = novoProcesso;
}

char* getPID(char* s){
	if(s == NULL)
		return NULL;

	char* ret_pid = malloc(strlen(s));

	strcpy(ret_pid, s);

	return ret_pid;
}

char* getPROGRAM(char* s) {

    char* program_start = strstr(s, "Program: ");
    if (program_start == NULL) {
        return NULL;  // PID não encontrado
    }

    program_start += strlen("Program: ");  // avança o ponteiro para o início do PROGRAM

    char* program_end = strchr(program_start, '\n');  // procura o fim do PROGRAM
    if (program_end == NULL) {
        return NULL;  // fim do PROGRAM não encontrado
    }

    size_t program_len = program_end - program_start;  // calcula o comprimento do PROGRAM

    char* program = (char*) malloc(program_len + 1);  // aloca memória para a string do PROGRAM

    strncpy(program, program_start, program_len);  // copia os caracteres do PROGRAM

    program[program_len] = '\0';  // adiciona o caracter nulo final
    
    return program;
}


char* getTIME_MS(char* s) {

    char* time_ms_start = strstr(s, "Time: ");
    if (time_ms_start == NULL) {
        return NULL;  // TIME_MS não encontrado
    }

    time_ms_start += strlen("Time: ");  // avança o ponteiro para o início do TIME_MS

    char* time_ms_end = strchr(time_ms_start, '\n');  // procura o fim do TIME_MS
    if (time_ms_end == NULL) {
        return NULL;  // fim do TIME_MS não encontrado
    }

    size_t time_ms_len = time_ms_end - time_ms_start;  // calcula o comprimento do TIME_MS

    char* time_ms = (char*) malloc(time_ms_len + 1);  // aloca memória para a string do TIME_MS

    strncpy(time_ms, time_ms_start, time_ms_len);  // copia os caracteres do TIME_MS

    time_ms[time_ms_len] = '\0';  // adiciona o caracter nulo final

    return time_ms;
}

int runExecute(Process* processo, char* program, char* pid, char* time_s){
	addProcessoExecuted(processo, program, pid, time_s);
	return 0;
}

int runStatus(char* pid, int fdToTracer){
	char* pid_s; char* program_s; char* time_s;
	char buffer[SIZE];

	pid_s = getPID(pid);
	program_s = getPROGRAM(pid);
	time_s = getTIME_MS(pid);

	ssize_t byte = sizeof(pid_s) + sizeof(program_s) + sizeof(time_s);
	ssize_t nbytes = sprintf(buffer,"%05lu%s %s %s\n", byte, pid_s, program_s, time_s);
	write(fdToTracer, buffer, nbytes);

	return 0;
}



/*
############################################################################

							// MAIN //

############################################################################
*/


int main(int argc, char *argv[]){

	//termina o servidor com ctrl+c
    signal(SIGINT,signal_handler_C);

    Process* processo;
	char buffer[1024];
	char* ficheiro = "tmp/ficheiro.txt";
	
	// Abrir fifos //

	int args_fifo = mkfifo("args_fifo",0666);

	if(args_fifo == -1){
		perror("Args fifo unopened");
		return -1;
	}

	
	while(1){

		int fdArgs = open("args_fifo",O_RDONLY);
		int fdFile = open(ficheiro, O_WRONLY | O_CREAT | O_APPEND, 0600);
		int read_bytes;

		read(fdArgs, buffer, 1);

		if(!strncmp( buffer,"U",1 )){	         // SE FOR A EXECUTE -U (U)
			read( fdArgs,buffer,6 );
			
			int i = 0;
			char* string[SIZE];
			while(buffer != NULL){
				string[i] = strtok(buffer, " ");
				i++;
				printf("%s\n",string[i]);
			}

			char* pid = string[1];
			char* program = string[2];
			char* time_s = string[3];


			if(!strncmp(buffer,"I",1)){          // I PARA INICIO, F PARA FIM
				runExecute(processo, program, pid, time_s);
			}
		}

		else if(!strncmp(buffer,"S",1)){         // SE FOR A STATUS (S)
			char* pid;
			int fdTracI = open("fif_m_i",O_RDONLY);
			runStatus(pid, fdTracI);
		}
		
		while((read_bytes = read(fdArgs, buffer, SIZE)) > 0){

			write(1,buffer,read_bytes);
		}

		close(fdArgs);
		close(fdFile);
	}
	return 0;
}



/*
void runStatus(Process processo, int fd)//obter informaçoes dos programas que estao a ser executados
{
    char *buffer = malloc(sizeof(char) * MAX);
    int nbytes;

    while (*processo != NULL) {
        if (processo -> exec == 0) {
            nbytes = sprintf(buffer, "PID: %s\tPrograma: %s\n", (*processo)->pid, (*processo)->program);
            write(fd, buffer, nbytes);
        }

        else if((*processo)->exec == 1){
            nbytes = sprintf(buffer, "PID: %s\tPrograma: %s\n", (*processo)->pid, (*processo)->program);
            write(fd, buffer, nbytes);
        }

        (*processo) = (*processo)->next;
    }

    free(buffer);
}*/
		