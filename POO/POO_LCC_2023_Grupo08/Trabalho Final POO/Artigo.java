import java.util.Random;
import java.io.Serializable;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.FileInputStream;
import java.io.File;

public abstract class Artigo implements Serializable
{
    private String codigo;
    private String descricao;
    private int exDonos;
    private double precobase;
    private double precocorrecao;
    private double estadoutilizacao;
    

    
    public Artigo(String codigo, String desc, int exdonos, double precobase, double aval){
        this.codigo=codigo;
        this.descricao=desc;
        this.precobase=precobase;
        this.exDonos=exdonos;
        this.estadoutilizacao=aval;
        this.precocorrecao = precobase - (precobase/exDonos * estadoutilizacao);
    }
    
    //METODOS GET
    public String getCodigo(){
        return this.codigo;
    }
    public String getDescricao(){
        return this.descricao;
    }
    public int getExDonos(){
        return this.exDonos;
    }
    public double getPrecoBase(){
        return this.precobase;
    }
    public double getPrecoCorrecao(){
        return this.precocorrecao;
    }
    public double getEstadoUtilizacao(){
        return this.estadoutilizacao;
    }
    

    //METODOS SET
    public void setCodigo(String x){
        this.codigo=x;
    }
    public void setDescricao(String desc){
        this.descricao=desc;
    }
    public void setExDonos(int exdonos){
        this.exDonos=exdonos;
    }
    public void setPrecoBase(double precobase){
        this.precobase=precobase;
    }
    public void setPrecoCorrecao(double precocorrecao){
        this.precocorrecao=precocorrecao;
    }
    public void setEstadoUtilizacao(double est){
        this.estadoutilizacao=est;
    }

    
    //METODO EQUALS
    public boolean equals(Object o){
        if(o==null || o.getClass()!=this.getClass()) return false;
        if(this==o) return true;
        Artigo art = (Artigo) o;
        return this.codigo.equals(art.getCodigo());

    }
    
}