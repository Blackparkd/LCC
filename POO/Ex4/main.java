package Ex4;

import java.util.Arrays;
import java.util.Scanner;

public class main
{
    public static void main(String[] args){
        Scanner in = new Scanner(System.in);
        ex classe = new ex(10);
        
        classe.adicionaString("Hello");
        classe.adicionaString("World");
        classe.adicionaString("!");
        classe.adicionaString("Buenos");
        classe.adicionaString("dias");
        classe.adicionaString("Matosinhos");
        classe.adicionaString("!");
        
        a(in, classe);
        b(in, classe);
        c(in, classe);
        d(in, classe);
        
    }
    
    private static void a(Scanner in, ex classe){
        System.out.println(Arrays.toString(classe.existentes()));
    }
    
    private static void b(Scanner in, ex classe){
        System.out.println("A maior string é: "+ (classe.maiorString()));
    }
    
    private static void c(Scanner in, ex classe){
        System.out.println("As strings que aparecem mais que uma vez são: "+ Arrays.toString(classe.repetidos()));
    }
    
    private static void d(Scanner in, ex classe){
        System.out.println("A string ocorre: "+ classe.ocorrencias("!")+" vezes");
    }
}
