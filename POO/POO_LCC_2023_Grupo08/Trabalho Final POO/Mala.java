

import java.time.LocalDate;
import java.io.Serializable;
import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.IOException;
import java.io.FileInputStream;
import java.io.FileOutputStream;

public class Mala extends Artigo implements Serializable
{ 
    private final String abs ="abs";//15%
    private final String polipropileno ="polipropileno";//20%
    private final String policarbonato = "policarbonato";//30%
    private final String poliester = "poliester";//35%
    private double volume;
    private int anocolecao;
    private String material;
    
    public Mala(String codigo, String desc, int exdonos, double precobase, double aval, double volume, int ano, String material){
        super(codigo, desc, exdonos, precobase, aval);
        this.volume=volume;
        this.anocolecao=ano;
        this.material=material;
        
        LocalDate agora = LocalDate.now();
        int year = agora.getYear();
        int preco=1;
        for(year=year; year>=ano; year--){
            preco++;
        }
        
        if(material.equals("abs")){
            super.setPrecoCorrecao(precobase-(precobase*preco/10)-(precobase-(precobase*preco/10))*0.15);
        }
        else if(material.equals("polipropileno")){
            super.setPrecoCorrecao(precobase-(precobase*preco/10)-(precobase-(precobase*preco/10))*0.20);
        }
        else if(material.equals("policarbonato")){
            super.setPrecoCorrecao(precobase-(precobase*preco/10)-(precobase-(precobase*preco/10))*0.30);;
        }
        else if(material.equals("poliester")){
            super.setPrecoCorrecao(precobase-(precobase*preco/10)-(precobase-(precobase*preco/10))*0.35);
        }
        
        
    }
    
    //METODOS GETS
    
    public double getVolume(){
        return this.volume;
    }
    public int getAnoColecao(){
        return this.anocolecao;
    }
    public String getMaterial(){
        return this.material;
    }
    
    //METODOS SET
  
    public void setVolume(double volume){
        this.volume=volume;
    }
    public void setAnoColecao(int ano){
        this.anocolecao=ano;
    }
    public void setMaterial(String material){
        this.material=material;
    }
    
    
    public boolean equals(Object o){
        return super.equals(o);
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
        sb.append("\tVolume= ").append(String.valueOf(this.volume)).append("\n");
        sb.append("\tAno de Colecao= ").append(String.valueOf(this.anocolecao)).append("\n");
        sb.append("\tMaterial= ").append(this.material).append("\n");
        
        return sb.toString();
    }
   
}
