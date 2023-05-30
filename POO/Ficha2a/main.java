package Ficha2a;


import java.util.Scanner;
import java.util.Arrays;
import static java.lang.Math.abs;

public class main
{
    
    
    public static void main(String args[])
    {
        // EX 1 //
        
        int num, n1, n2,min;
        Scanner input = new Scanner(System.in);
        System.out.print("1º Array: Ler quantos inteiros? ");
        n1 = input.nextInt();
        int[] array1 = new int[n1];
        
        
        for(int x = 0; x < n1; x++)
        {
            System.out.print("Escrever inteiro: ");
            num = input.nextInt();
            array1[x] = num;
        }
        
        // a)
        int menor = minimo(array1, n1);
        System.out.println("O mínimo é: "+ menor);
       
        // b)
        System.out.print("Insira o índice inicial: ");
        int i = input.nextInt();
        System.out.print("Insira o índice final: ");
        int j = input.nextInt();
        
        int[] arrayDelim = new int[n1];
        arrayDelim = indices(array1, i, j);
        System.out.print("Array resultante: ");
        for(int x = 0; x < (j-i+1); x++){
            System.out.print(array1[x]+" ");
        }
        System.out.println();
        
        // c)
        System.out.print("2ºArray: Ler quantos inteiros? ");
        n2 = input.nextInt();
        
        int[] array2 = new int[n2];
        int[] arrayComum = new int[n2];
        int num2;
        
        for(int x = 0; x < n2; x++)
        {
            System.out.print("Escrever inteiro: ");
            num2 = input.nextInt();
            array2[x] = num2;
        }
        arrayComum = comuns(array1, n1, array2, n2);
        System.out.print("Os elementos comuns entre os dois arrays são: ");
        for(int x = 0; x < n2; x++){
            System.out.print(arrayComum[x] + " ");
        }        
    }
    
    // 1a)
    public static int minimo(int[] array, int n)
    {
        int min = metodos.minimo(array, n);
        return min;
    }
    
    // 1b)
    public static int[] indices(int[] array, int i, int j){
        int[] r = new int[j-i+1];
        r = metodos.buildArray(array, i, j);
        return r;
    }
    
    // 1c)
    public static int[] comuns(int[] array1, int n1, int[] array2, int n2){
        int[] r = new int[n1];
        r = metodos.comuns(array1, n1, array2, n2);
        return r;
    }
}
