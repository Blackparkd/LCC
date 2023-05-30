package Ficha2a;

public class metodos
{
    public static int minimo(int [] array, int n) {
        int menor = Integer.MAX_VALUE;
        for (int i = 0; i < n; i++) {
            if (array[i] < menor)
                menor = array[i];
        }
        return menor;
    }

    public static int[] buildArray(int[] array, int i, int j){
        int n = j-i+1, x;
        int[] r = new int[n];
        for(x=i; x < n; x++){
            r[x] = array[i+x];
        }
        return r;
    }

    public static int[] comuns(int[] a, int na, int[] b, int nb){
        int[] r = new int[nb];
        int i = 0, j = 0, z = 0;
        for(i=0; i < na; i++){
            for(j=0; j < nb; j++){
                if(a[i] == b[j]){
                    r[z] = a[i];
                    z++;
                }
            }
        }
       return r;
    }
}
