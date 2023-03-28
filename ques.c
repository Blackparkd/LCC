#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define SIZE 1024

int maior();
int media();
int segundoM();
int bitsUm();
int trailingZ();
int qDig();
char *strcat2();
char *strcpy2();
int strcmp2();
char *strstr2();
void streev();
void strnoV();
void truncW();
char charMaisfreq();
int iguaisConsecutivos();
int difConsecutivos();
int maiorPrefixo();
int maiorSufixo();
int sufPref();
int contaPal();
int contaVogais();
int contida();
int palindorome();
int remRep();
int limpaEspacos();



int main()
{
	int pergunta, i=0;
	char s1[SIZE];
	char s2[SIZE];
	char s3[SIZE];
	char s4[SIZE];

	strcpy(s1,"Hello ");
	strcpy(s2,"World!\n");
	strcpy(s3,"Texto ");
	strcpy(s4,"random.\n");

	do
	{
		printf("Pergunta: ");
		scanf("%d",&pergunta);
		printf("\n#########################\n\n");

		switch(pergunta)
		{

		case 0: printf("End.\n");
				break;
		
		case 1: maior();
				break;

		case 2: media();
				break;

		case 3: segundoM();
				break;

		case 4: bitsUm(5);
				break;

		case 5: trailingZ(256);
				break;

		case 6: qDig(2541);
				break;

		case 7: strcat2(s1,s2);
				break;

		case 8: strcpy2(s1,s2);
				break;

		case 9: strcmp2(s1,s2);
				break;

		default: main();
		}
	}
	while(pergunta != 0);

	return 0;
}
	




// 1)

int maior()
{
	int n=1;
	int maior = 0;
	int i = 0;

	while (n != 0)
	{
		printf("Insira um número (sequência acaba quando inserir '0'): ");
		scanf("%d",&n);
		if( n > maior)
		{
			maior = n;
		}
		i++;
	}
	putchar('\n');
	printf("O número maior da sequência é: %d.\n",maior);
	return maior;
}

// 2)

int media()
{
	int n = 1;
	int numero = 0;
	int media = 0;
	int i = 0;
	int soma = 0;

	while (n != 0)
	{
		printf("Insira um número (sequência acaba quando inserir '0'): ");
		scanf("%d",&n);
		soma += n;
		numero++;
		i++;
	}
	media = soma/(numero-1);
	putchar('\n');
	printf("A média da sequência é: %d.\n",media);
	return media;
}

// 3)

int segundoM()
{
	int n=1;
	int maior = 0;
	int segMaior = 0;
	int i = 0;

	while (n != 0)
	{
		printf("Insira um número (sequência acaba quando inserir '0'): ");
		scanf("%d",&n);
		if( n > maior)
		{
			segMaior = maior;
			maior = n;
		}
		i++;
	}
	putchar('\n');
	printf("O segundo maior número da sequência é: %d.\n",segMaior);
	return segMaior;	
}

// 4)

int bitsUm(unsigned int n)
{
	int bits = 0;
	
	while (n > 0)
	{
		if(n%2 != 0)
		{
			bits++;
		}
		n/=2;
	}
	//printf("%d\n",bits);
	return bits;
}

// 5)

int trailingZ(unsigned int n)
{
	int acc = n;
	int bits = 0;

	/*while(n > 0)
	{
		if(n%2 == 0)
		{
			bits++;
		}
		n/=2;
	}*/
	bits = 32 - bitsUm(acc);
	printf("O número %d tem %d bits a 0 e %d bits a um.\n",acc,bits,bitsUm(acc));
	return bits;
}

// 6)

int qDig(unsigned int n)
{
	int digitos = 0;

	while(n > 0)
	{
		digitos++;
		n/=10;
	}
	printf("Número de digitos: %d.\n",digitos);
	return digitos;
}

// 7)

char *strcat2(char s1[],char s2[])
{
	int tam1 = strlen(s1);

	strcpy2(s1+tam1,s2);

	printf("%s",s1);

	return s1;
}

// 8)

char *strcpy2(char s1[], char s2[])
{
	int i;
	for( i = 0; s2[i]!='\0'; i++)
	{
		s1[i] = s2[i];
	}
	s1[i] = '\0';

	//printf("%s",s1);
	
	return s1;
}

// 9)

int strcmp2(char s1[], char s2[])
{
	return 0;
}
