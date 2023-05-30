package Ex3;

public class ex{
    
    public static void ordena(int[] array){
        int menor;
        
        for(int i=0; i < array.length; i++){
            menor = i;
            for(int j=i; j < array.length; j++){
                if(array[j] < array[menor]){
                    menor = j;
                }
            swap(array, i, menor);
            }
        }
    }
    
    public static void swap(int[] array, int i, int j){
        int t = array[i];
        array[i] = array[j];
        array[j] = t;
    }
    
    public static int binarySearch(int[] array, int N, int x){
        int i=0, m, min=0, r=0, flag=0;
        m = N/2;
        while(min < N && flag == 0){
            if(x == array[m]){
                r = m;
                flag = 1;
            }
            else if(x < array[m]){
                N = m-1;
            }
            else
                min = m+1;
            
            m = (min+N)/2;
        }
        if(min == N && array[min] == x)
            r = min;
            
        return r;
    }
}
