import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * Write a description of class SapatilhaPremium here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class SapatilhaPremium extends Sapatilha implements Serializable
{
    private String autor;
    
    public SapatilhaPremium (String codigo, String desc, int exdonos, double precobase, double aval, int tamanho, boolean tematacadores, String cor, int anocolecao, String autor){
        super(codigo, desc, exdonos, precobase, aval, tamanho, tematacadores, cor, anocolecao);
        this.autor=autor;
        
        LocalDate data = LocalDate.now();
        int ano=data.getYear();
        int preco=0;
        while(ano>super.getAnoColecao()){
            preco+=3;
            ano--;
        }
        super.setPrecoCorrecao(precobase+(precobase*aval*exdonos));
        
    }
    
    public String getAutor(){
        return this.autor;
    }
    public void setAutor(String autor){
        this.autor=autor;
    }
    
    public String toString(){
        final StringBuffer sb = new StringBuffer();
        sb.append("\t").append(this.getClass().getName().toString()).append("\n");
        sb.append("\tCodigo= ").append(super.getCodigo()).append("\n");
        sb.append("\tDescricao= ").append(super.getDescricao()).append("\n");
        sb.append("\tNumero de Ex donos= ").append(String.valueOf(super.getExDonos())).append("\n");
        sb.append("\tPreco Base= ").append(String.valueOf(super.getPrecoBase())).append("\n");
        sb.append("\tPreco correcao= ").append(String.valueOf(super.getPrecoCorrecao())).append("\n");
        sb.append("\tAvalicacao= ").append(String.valueOf(super.getEstadoUtilizacao())).append("\n");
        sb.append("\tTamanho= ").append(String.valueOf(super.getTamanho())).append("\n");
        if(super.getTemAtacadores()==true){
            sb.append("\tCom atacadores\n");
        }
        else{
            sb.append("\tSem atacadores\n");
        }
        sb.append("\t").append(super.getCor()).append("\n");
        sb.append("\tAno da Colecao= ").append(String.valueOf(super.getAnoColecao())).append("\n");
        sb.append("\tAutor da Colecao= ").append(this.autor).append("\n");
        return sb.toString();
    }
    
    public boolean equals(Object o){
        return super.equals(o);
    }
}