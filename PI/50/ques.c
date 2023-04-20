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
void strrev();
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
	int pergunta; //i=0;
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

		case 5: trailingZ(11);
				break;

		case 6: qDig(2541);
				break;

		case 7: strcat2(s1,s2);
				break;

		case 8: strcpy2(s1,s2);
				break;

		case 9: strcmp2(s1,s2);
				break;

		case 10: strstr2(s1,s2);
				break;

		case 11: strrev(s1);
				break;
		
		case 12: strnoV(s1);
				break;
		
		case 13: truncW(s3,s1);
				break;
		
		case 14: charMaisfreq(s1);
				break;
		
		case 15: iguaisConsecutivos(s1);
				break;
		
		case 16: difConsecutivos(s1);
				break;

		case 17: maiorPrefixo(s1,s2);
				break;

		case 18: maiorSufixo(s1,s2);
				break;

		case 19: sufPref(s1,s2);
				break;

		case 20: contaPal(s1);
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

	printf("%s",s1);
	
	return s1;
}

// 9)

int strcmp2(char s1[], char s2[])
{
	/*int i=0,n;
	char compara[SIZE];

	do
	{
		n = s1[i] - s2[i];
		//printf("%d e %d",s1[i],s2[i]);
		i++;
	}
	while(n == 0 && s1[i] != '\0');

	if(n < 0)
	{
		strcpy(compara,"<0\n");
	}
	else if(n>0)
	{
		strcpy(compara,">0\n");
	}
	else
	{
		strcpy(compara,"=0\n");
	}

	printf("%s",compara);
	return n;
	*/

	int n,i;

	for(i=0;s1[i]!='\0' && s2[i]!='\0';i++)
	{
		if (s1[i] < s2[i])
			n = -1;
		else if (s1[i] > s2[i])
			n = 1;
		else
			n = 0;
	}
	//printf("%d\n\n",n);
	return n;
}

// 10)  10/10 Testes

char *strstr2(char s1[], char s2[])
{
	int i=0, j=0,flag;

	if(s1[0] == '\0' || s2[0] == '\0')
		return s1;

	if(strlen(s1)<strlen(s2)) NULL;

	while(s1[i] && s2[j])
	{
		if(s1[i] == s2[j])
		{
			if(j==0) flag = i;
			
			i++;
			j++;
			printf("%d\n",flag);
		}
		else
		{
			i++;
			j=0;
		}
	}
	printf("%s\n",s1+flag);

	if(j==0)
		return NULL;

	else
		return s1+flag;
}

// 11) 	10/10 testes

void strrev(char s1[])
{
	int i, t = 0;
	int n = strlen(s1);

	for(i=0; i < n; i++)
	{	
		n--;
		t = s1[i];
		s1[i] = s1[n];
		s1[n] = t;
	}

}

// 12) 

int eVogal(char c)
{
	if(c == 'a'||c == 'A'||c == 'e'||c == 'E'||c == 'i'||c == 'I'||c == 'o'||c == 'O'||c == 'u'||c == 'U')
	{
		ret = 1;
	}
	else 
	{
		ret = 0;
	}
	return ret;
}

void strnoV(char s1[])
{
	int i,j=0;
	n = strlen(s1);
	char r[n];

	for(i=0;s1[i]!='\0';i++)
	{
		if(!eVogal(s1[i]))
		{
			r[j] = s1[i];
			j++;
		}
	}
	r[j] = '\0';

	for(i=0;r[i]!='\0';i++)
	{
		s1[i] = r[i];
	}
	s1[i] = '\0';
}
