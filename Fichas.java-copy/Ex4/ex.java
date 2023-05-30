package Ex4;


public class ex{
    
    private String[] array;
    private int tamanho;
    private int ocupacao;
    
    public ex(int tam){
        this.array = new String[tam];
        this.tamanho = tam;
    }
    
    public boolean adicionaString(String a){
        if(this.ocupacao == this.tamanho)
            return false;
            
        this.array[ocupacao] = new String(a);
        ocupacao ++;
        
        return true;
    }
    
    public String[] existentes(){
        String[] r = new String[this.ocupacao];
        int x = 0;
        for(int i = 0; i < this.ocupacao; i++){
            boolean repetido = false;
            for(int j = 0; j < x; j++){
                if(r[j].equals(this.array[x])){
                    repetido = true;
                    break;
                }
            }
            if(!repetido)
                    r[x++] = new String(this.array[i]);
        }
        
        String[] res = new String[x];
        System.arraycopy(array, 0, res, 0, x);
        
        return res;
    }
    
    public String maiorString(){
        String r = null;
        int maior = -1;
        
        for(int i = 0; i < this.ocupacao; i++){
            String elem = this.array[i];
            if(elem.length() > maior){
                maior = elem.length();
                r = elem;
            }
        }
        return new String(r);
    }
    
    public String[] repetidos(){
        String[] array = new String[this.ocupacao];
        int p = 0;
        for(int i = 0; i < this.ocupacao; i++){
            if(this.repete(this.array[i]) && !this.repeteAte(this.array[i], array, p)){
                array[p++] = this.array[i];
            }
        }
        String[] res = new String[p];
        System.arraycopy(array, 0, res, 0, p);
        return res;
    }
    
    public int ocorrencias(String a){
        int oc = 0;
        for(int i = 0; i < this.ocupacao; i++){
            if(this.array[i].equals(a))
                oc ++;
        }
        return oc;
    }
    
    public boolean repete(String a){
        return this.ocorrencias(a) > 1;
    }
    
    public boolean repeteAte(String a, String[] array, int n){
        int oc = 0;
        for(int i = 0; i < n && oc <= 1; i++){
            if(a.equals(array[i]))
                oc++;
        }
        return oc > 0;
    }
}
