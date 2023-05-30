package Ex5;


/**
 * Escreva uma descrição da classe ex aqui.
 * 
 * @author (seu nome) 
 * @version (um número da versão ou uma data)
 */
public class ex
{
    private int[][] notasTurma;
    private int tam1;
    private int tam2;
    
    public ex(int[][] input){
        this.tam1 = input.length;
        this.tam2 = input[0].length;
        notasTurma = new int[tam1][tam2];
        
        for(int i = 0; i < this.tam1; i++){
            System.arraycopy(input[i], 0, this.notasTurma[i], 0, tam2);
        }
    }
    
    public ex soma(ex second){
        if(this.tam1 == second.tam1 && this.tam2 == second.tam2){
            ex soma = new ex(this.notasTurma);
            for(int i = 0; i < this.tam1; i++){
                for(int j = 0; i < this.tam2; j++){
                    soma.notasTurma[i][j] += second.notasTurma[i][j];
                }
            }
            return soma;
        }
        else {
            return null;
        }
    }
}
