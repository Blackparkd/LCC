import java.time.LocalDate;
import java.io.Serializable;
import java.io.ObjectOutputStream;
import java.io.IOException;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.FileInputStream;

/**
 * Write a description of class Sapatilhas here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class Sapatilha extends Artigo implements Serializable
{
    private int tamanho;
    private boolean tematacadores;
    private String cor;
    private int anocolecao;
    
    public Sapatilha(String codigo, String desc, int exdonos, double precobase, double aval, int tamanho, boolean tematacadores, String cor, int anocolecao){
        super(codigo, desc, exdonos, precobase, aval);
        this.tamanho=tamanho;
        this.cor=cor;
        this.anocolecao=anocolecao;
        this.tematacadores=tematacadores;
        
        LocalDate data = LocalDate.now();
        int year = data.getYear();
        

        if(super.getExDonos()>0 ){
            super.setPrecoCorrecao(precobase*aval);
        }
        else if(super.getExDonos()==0 && this.tamanho>45 && this.anocolecao<year){
            super.setPrecoCorrecao(precobase/2);
        }
        
        else{super.setPrecoCorrecao(precobase);}
    }
    
    //METODOS GET
    public int getTamanho(){
        return this.tamanho;
    }
    public boolean getTemAtacadores(){
        return this.tematacadores;
    }
    public String getCor(){
        return this.cor;
    }
    public int getAnoColecao(){
        return this.anocolecao;
    }
    
    //METODOS SET
    public void setTamanho(int tamanho){
        this.tamanho=tamanho;
    }
    public void setTemAtacadores(boolean tematacadores){
        this.tematacadores=tematacadores;
    }
    public void setCor(String cor){
        this.cor=cor;
    }
    public void setAnoColecao(int ano){
        this.anocolecao=ano;
    }
    //METODO EQUALS
    public boolean equals(Object o){
        return super.equals(o); 
    }
    
   
    
    //TO String
    public String toString(){
        final StringBuffer sb = new StringBuffer();
        sb.append("\t").append(this.getClass().getName().toString()).append("\n");
        sb.append("\tCodigo= ").append(super.getCodigo()).append("\n");
        sb.append("\tDescricao= ").append(super.getDescricao()).append("\n");
        sb.append("\tNumero de Ex donos= ").append(String.valueOf(super.getExDonos())).append("\n");
        sb.append("\tPreco Base= ").append(String.valueOf(super.getPrecoBase())).append("\n");
        sb.append("\tPreco correcao= ").append(String.valueOf(super.getPrecoCorrecao())).append("\n");
        sb.append("\tAvalicacao= ").append(String.valueOf(super.getEstadoUtilizacao())).append("\n");
        sb.append("\tTamanho= ").append(String.valueOf(this.tamanho)).append("\n");
        if(this.tematacadores==true){
            sb.append("\tCom atacadores\n");
        }
        else{
            sb.append("\tSem atacadores\n");
        }
        sb.append("\t").append(this.cor).append("\n");
        sb.append("\tAno da Colecao= ").append(String.valueOf(this.anocolecao)).append("\n");
        return sb.toString();
    }
  
    
}