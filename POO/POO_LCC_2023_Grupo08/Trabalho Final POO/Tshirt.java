import java.io.ObjectOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.FileInputStream;
import java.io.Serializable;

/**
 * Write a description of class Tshirt here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class Tshirt extends Artigo implements Serializable
{
    private String tamanho;
    private String padrao;
    
    public Tshirt(String codigo, String desc, int exdonos, double precobase, double aval, String tamanho, String padrao){
        super(codigo, desc, exdonos, precobase, aval);
        if(tamanho.equals("S")) this.tamanho="S";
        if(tamanho.equals("M")) this.tamanho="M";
        if(tamanho.equals("L")) this.tamanho="L";
        if(tamanho.equals("XL")) this.tamanho="XL";
        
        if(padrao.equals("liso")) this.padrao="liso";
        if(padrao.equals("riscas")) this.padrao="riscas";
        if(padrao.equals("palmeiras")) this.padrao="palmeiras";
        
        if((padrao.equals("liso") || padrao.equals("palmeiras")) && super.getExDonos()>0){
            super.setPrecoCorrecao(precobase/2);
        }
        else{super.setPrecoCorrecao(precobase);}
    }
    
    //METODOS GET
    public String getTamanho(){
        return this.tamanho;
    }
    public String getPadrao(){
        return this.padrao;
    }
    
    //METODOS SET
    public void setTamanho(String tamanho){
        this.tamanho=tamanho;
    }
    public void setPadrao(String padrao){
        this.padrao=padrao;
    }
    

    //TO STRING
    public String toString(){
        final StringBuffer sb = new StringBuffer();
        sb.append("\t").append(this.getClass().getName().toString()).append("\n");
        sb.append("\tCodigo= ").append(super.getCodigo()).append("\n");
        sb.append("\tDescricao= ").append(super.getDescricao()).append("\n");
        sb.append("\tNumero de Ex donos= ").append(String.valueOf(super.getExDonos())).append("\n");
        sb.append("\tPreco Base= ").append(String.valueOf(super.getPrecoBase())).append("\n");
        sb.append("\tPreco correcao= ").append(String.valueOf(super.getPrecoCorrecao())).append("\n");
        sb.append("\tAvalicacao= ").append(String.valueOf(super.getEstadoUtilizacao())).append("\n");
        sb.append("\t").append(this.tamanho).append("\n");
        sb.append("\t").append(this.padrao).append("\n");
        return sb.toString();
    }
    
    public boolean equals(Object o){
        return super.equals(o);
    }
   
}