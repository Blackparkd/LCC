
/**
 * Escreva uma descrição da classe Exercicios aqui.
 * 
 * @author (seu nome) 
 * @version (um número da versão ou uma data)
 */

import java.util.Scanner;
import java.time.LocalDateTime;
import java.util.*;
import static java.lang.Math.abs;
import static java.lang.Math.sqrt;

public class Ficha1
{
    // MAIN //
    
    public static void main(String[] args)
    {
        int dia = 14;
        int mes = 03;
        int ano = 2023;
        
        int array[] = {12,5,3,16,20,4,5,6,15,19};
        
        int temperaturas[] = {30,20,12,1,-2,-4,-5,10};
        
        // EX 1 //
        
        System.out.println("O número de dias é " + dias(dia,mes,ano) + ".");
        System.out.println("O dia da semana é " +  diaSemana(dia,mes,ano) + ".");
        System.out.printf("\n\n");
        
        // EX 2 //
        
        System.out.println("Cenas" + montaData(mes,ano,397));
        System.out.printf("\n\n");
        
        // EX 3 //
        
        System.out.println("Classificações: " + Arrays.toString(nClassifica(10,array)));
        System.out.printf("\n\n");
    
        // EX 4 //
    
        int r[] = media(8,temperaturas);
        System.out.println("A média das "+ 8 +" temperaturas foi de "+ r[0] +" graus.");
        System.out.print("A maior variação registou-se entre os dias "+ r[1] +" e "+ (r[1]+1) +", tendo a temperatura ");
        if(r[2] > 0)
        {
            System.out.print("subido ");
        }
        else
        {
            System.out.print("descido ");
        }
        System.out.println(abs(r[2]) + " graus.");
        
        // EX 5 //
        
        areaPerimetro();
        
        // EX 6 //
        
        primo();
        
        // EX 7 //
        
        idade();
    }
    
    
    
    
    
    // EX 1 //
    
    public static int dias(int diaX, int mesX, int anoX)
    {
        int dia = 01;
        int mes = 01;
        int ano = 1900;
        
        int diaSemana = (anoX-ano)*365;
        diaSemana += (anoX-ano)/4;
        
        if(anoX%4 == 0 && mesX < 3)
        {
            diaSemana -= 1;
        }
        
        for(int i = 1; i < mesX; i++)
        {
            diaSemana += 30;
            
            if(i == 1 || i == 3 || i == 5 || i == 7 || i == 8 || i == 10 || i == 12)
            {
                diaSemana ++;
            }
            else if(i == 2)
            {
                diaSemana -= 2;
            }
        }
        diaSemana += diaX;
        return (diaSemana);
    }
    
    public static String diaSemana(int dia, int mes, int ano)
    {
        int nDias = dias(dia,mes,ano);
        return Semana(nDias % 7);
    }
    
    private static String Semana(int dia)
    {
        switch(dia)
        {
            case 0 : return "Domingo";
            case 1 : return "Segunda-Feira";
            case 2 : return "Terça-Feira";
            case 3 : return "Quarta-Feira";
            case 4 : return "Quinta-Feira";
            case 5 : return "Sexta-Feira";
            case 6 : return "Sábado";
        }
        return "Nope";
    }
    
    
    // EX 2 //
    
    public static String somaMes(int mes1, int ano1, int numDias)
    {
        int mes = 0, ano = 0;
        
        int ano2 = numDias/365;
        int mes2 = numDias%365;
        
        ano = ano1 + ano2;
        mes = mes1 + mes2;
        
        if(mes > 12)
        {
            int dif = mes/12;
            mes = 1;
            ano ++;
        }
        String datas = mes + "/" + ano;
        
        return datas;
    }
    
    public static String montaData(int mes1, int ano1, int data)
    {
        String datas = "M/A -->" + somaMes(mes1,ano1,data);
        
        return datas;
    }
    
    
    
    // EX 3 //
    
    public static int[] nClassifica(int n, int[] classificacao)
    {
        int[] intervalos = {0,0,0,0};
        
        for(int i = 0; i < n; i++)
        {
            if(classificacao[i] >= 0 && classificacao[i] < 5)
            {
                intervalos[0] ++;
            }
            else if(classificacao[i] >= 5 && classificacao[i] < 10)
            {
                intervalos[1] ++;
            }
            else if(classificacao[i] >= 10 && classificacao[i] < 15)
            {
                intervalos[2] ++;
            }
            else
            {
                intervalos[3] ++;
            }    
        }
        return intervalos;
    }
    
    
    // EX 4 //
    
    private static int[] media(int n, int temperaturas[])
    {
        int[] resultado = {temperaturas[0],0,-1}; //Media, dia, variação//
        for(int i = 1; i < n; i++)
        {
            resultado[0] += temperaturas[i];
            
            int diferenca = temperaturas [i] - temperaturas[i-1];
            
            if(diferenca > resultado[2])
            {
                resultado[1] = i;
                resultado[2] = diferenca;
            }
        }
        resultado[0] /= n;
        return resultado;
    }
    
    
    // EX 5 //
    
    private static int areaPerimetro()
    {
        double base = 1, altura = 0, hipotenusa = 0;
        double area = 0, perimetro = 0;
        Scanner input = new Scanner(System.in);
        
        while(base != 0.0)
        {
            System.out.print("Base: ");
            base = input.nextDouble();
            System.out.print("Altura: ");
            altura = input.nextDouble();
            
            hipotenusa = sqrt((Math.pow(base,2)) + (Math.pow(altura,2)));
            
            area = (base * altura)/2;
            
            perimetro = (base + altura + hipotenusa);
            
            System.out.printf("Área: %.5f\nPerimetro: %.5f\n",area,perimetro);
        }
        System.out.println("Base = 0.0 || Programa acaba!");
        return 0;
    }
    
    
    // EX 6 //
    
    private static boolean ePrimo(int n)
    {
       int i;
       boolean val = true;
       
       for(i = 2; i < n && val == true; i++)
       {
           if(n%i == 0)
           {
               val = false;
           }
       }
       return val;
    }
    
    private static void primo()
    {
        int n = 1;
        int i;
        char again = 's';       
        
        while(again == 's')
        {
            Scanner input = new Scanner(System.in);        
            
            System.out.print("Inserir número: ");
            n = input.nextInt();
        
            for(i = 2; i < n; i++)
            {
                if (ePrimo(i))
                {
                    System.out.printf("Primo:%d\n\n",i);
                }
            }
    
            System.out.print("Jogar mais? (s/n) ");
            again = input.next().charAt(0);
        }
    }
    
    
    // EX 7 //
    
    private static int idade()
    {
        Scanner input = new Scanner(System.in);
        System.out.print("Dia ");
        int dia = input.nextInt();
        System.out.print("Mês: ");
        int mes = input.nextInt();
        System.out.print("Ano: ");
        int ano = input.nextInt();

        int diaAtual = LocalDateTime.now().getDayOfMonth();
        int mesAtual = LocalDateTime.now().getMonthValue();
        int anoAtual = LocalDateTime.now().getYear();
        int horAtual = LocalDateTime.now().getHour();
        int minAtual = LocalDateTime.now().getMinute();
        
        int difAno = anoAtual - ano;
        
        if(difAno < 0)
        {
            System.out.println("Idade inválida.");
            return 0;
        }
        
        int horas;
        
        horas = difAno * 365 * 24 + (anoAtual - ano)/4;
        
        for(int i = mes ; i < mesAtual; i++)
        {
            horas += 30*24;
            if(i == 1 || i == 3 || i == 5 || i == 7 || i == 8 || i == 10)
            {
                horas += 24;
            }
            else if( i == 2)
            {
                horas -= 48; // menos dois dias de fevereiro -- 48 horas
            }
        }
        
        for(int i = dia; i <= diaAtual; i++)
        {
            horas += 24;
        }
        
    System.out.println("Diferença em horas: "+ horas);
    System.out.printf("Altura do cálculo: %d/%d/%d às %d:%2d horas.\n",diaAtual,mesAtual,anoAtual,horAtual,minAtual);
        
        return 0;
    }
}
























