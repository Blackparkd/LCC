package Ex5;


import java.util.Arrays;


public class main
{
   public static void main(String[] args){
       int[][] matriz = new int[5][5];
       for(int i = 0; i < 4; i++){
           for(int j = 0; j < 4; j++){
               matriz[i][j] = 1;
           }
       }
       ex notasTurma = new ex(matriz);
       
       ex soma = notasTurma.soma(notasTurma);
   }
   
   
}
