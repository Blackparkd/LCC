package Ex2;

import java.util.Scanner;
import java.time.LocalDateTime;
import java.time.LocalDate;
import java.util.*;
import static java.lang.Math.abs;
import static java.lang.Math.sqrt;


public class main{
    
    public static void main(String[] args){
        ex2 datas = new ex2(10);
        Scanner in = new Scanner(System.in);
        
        adicionarData(in, datas);
        adicionarData(in, datas);
        adicionarData(in, datas);
        
        
        System.out.print("Dia: ");
        int dia = in.nextInt();
        System.out.print("Mês: ");
        int mes = in.nextInt();
        System.out.print("Ano: ");
        int ano = in.nextInt();
        
        LocalDate data = LocalDate.of(ano,mes,dia);
        
        System.out.println("Data mais próxima: "+ datas.dataMaisProxima(data));
        
        System.out.println(datas.toString());
    }
    
    private static void adicionarData(Scanner in, ex2 datas){
        System.out.print("Dia: ");
        int dia = in.nextInt();
        System.out.print("Mês: ");
        int mes = in.nextInt();
        System.out.print("Ano: ");
        int ano = in.nextInt();
        
        LocalDate data = LocalDate.of(ano,mes,dia);
        
        datas.insereData(data);
    }
}
