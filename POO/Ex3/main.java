package Ex3;

import java.util.Scanner;

public class main
{
    
    public static void main(String args[])
    {
        int num, n;
        Scanner input = new Scanner(System.in);
        System.out.print("1º Array: Ler quantos inteiros? ");
        n = input.nextInt();
        int[] array = new int[n];
        
    
        for(int i = 0; i < n; i++){
            System.out.print("Escrever inteiro: ");
            num = input.nextInt();
            array[i] = num;
        }
        
        ordena(array);
        
        System.out.print("O array ordenado é: ");
        for(int i = 0; i < n; i++){
            System.out.print(array[i] + " ");
        }
        System.out.println();
        System.out.println();
        int r = binarySearch(array, 3);
        if(r == -1)
            System.out.println("O número não existe no array dado");
        else
            System.out.println("O número está na posição: " + r);
        
    }
    
    public static void ordena(int[] array){
        ex.ordena(array);
    }
    
    public static int binarySearch(int[] array,int x){
        int n = array.length;
        int r = ex.binarySearch(array, n, x);
        return r;
    }
}
