import java.io.Serializable;
import java.time.LocalDate;

/**
 * Write a description of class MalaPremium here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class MalaPremium extends Mala implements Serializable
{
    private String autor;
    
    public MalaPremium(String codigo, String desc, int exdonos, double precobase, double aval, double volume, int ano, String material, String autor){
        super(codigo, desc, exdonos, precobase, aval, volume, ano, material);
        this.autor=autor;
        LocalDate agora = LocalDate.now();
        int year = agora.getYear();
        double preco=1;
        for(year=year; year>=ano; year--){
            preco++;
        }
        
        super.setPrecoCorrecao(precobase+(precobase*(preco/10)));
    }
    
    public String getAutor(){
        return this.autor;
    }
    
    public void setAutor(String autor){
        this.autor=autor;
    }
    
    public boolean equals(Object o){
        return super.equals(o);
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
        sb.append("\tVolume= ").append(String.valueOf(super.getVolume())).append("\n");
        sb.append("\tAno de Colecao= ").append(String.valueOf(super.getAnoColecao())).append("\n");
        sb.append("\tMaterial= ").append(super.getMaterial()).append("\n");
        sb.append("\tNome do Autor da Colecao= ").append(this.autor).append("\n");
        
        return sb.toString();
    }
    
}
