import java.io.Serializable;
import java.io.ObjectOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.FileInputStream;
import java.time.LocalDateTime;
import java.util.ArrayList;


public class Transportador implements Serializable
{
    private String nome;
    private double valorbasePequeno;
    private double valorbaseMedio;
    private double valorbaseGrande;
    private double imposto;
    private double precostotais;
    private ArrayList<Encomenda> encomendas;
    private int numencs;
    
    public Transportador(String nome, double valorp, double valorm, double valorg, double imposto){
        this.nome=nome;
        this.valorbasePequeno=valorp;
        this.valorbaseMedio=valorm;
        this.valorbaseGrande=valorg;
        this.imposto=imposto;
        this.precostotais=0;
        ArrayList<Encomenda> encomendas = new ArrayList<Encomenda>();
        this.encomendas=encomendas;
        this.numencs=0;
    }
    
    //METODOS GET
    public String getNome(){
        return this.nome;
    }
    public double getValorBasePequeno(){
        return this.valorbasePequeno;
    }
    public double getValorBaseMedio(){
        return this.valorbaseMedio;
    }
    public double getValorBaseGrande(){
        return this.valorbaseGrande;
    }
    public double getImposto(){
        return this.imposto;
    }
    public double getPrecosTotais(){
        return this.precostotais;
    }
    public ArrayList<Encomenda> getEncomendas(){
        return this.encomendas;
    }
    public int getNumEncs(){
        return this.numencs;
    }
    
    //METODOS SET
    public void setNome(String nome){
        this.nome=nome;
    }
    public void setValorBasePequeno(double v){
        this.valorbasePequeno=v;
    }
    public void setValorBaseMedio(double v){
        this.valorbaseMedio=v;
    }
    public void setValorBaseGrande(double v){
        this.valorbaseGrande=v;
    }
    public void setImposto(double imposto){
        this.imposto=imposto;
    }
    public void setPrecosTotais(double precostotais){
        this.precostotais=precostotais;
    }
    public void setEncomendas(ArrayList<Encomenda> encomendas){
        this.encomendas=encomendas;
    }
    public void setNumEncs(int numencs){
        this.numencs=numencs;
    }
    
    //OUTROS METODOS
    public void addEncomenda(Encomenda e){
        this.encomendas.add(e);
    }
    public void removeEncomenda(Encomenda e){
        this.encomendas.remove(e);
    }
    
    public void encomendaChegou( Encomenda e){
        for(Encomenda enc : this.encomendas){
            if(e.equals(enc)){
                e.entregue();
                this.precostotais+=this.precostotais+(precoExpedicao(e)-(precoExpedicao(e)*this.imposto));
            }
        }
    }
    
    public double precoExpedicao(Encomenda e){
        if(e.getTamanho().equals("pequena")){
            return (this.valorbasePequeno);
        }
        if(e.getTamanho().equals("media")){
            return (this.valorbaseMedio);
        }
        return (this.valorbaseGrande);
    }
    
   
    
    //METODO EQUALS
    public boolean equals(Object o){
        if(this==o) return true;
        if(o==null || o.getClass()!=this.getClass()) return false;
        Transportador t = (Transportador) o;
        return this.nome.equals(t.getNome()) && this.numencs==t.getNumEncs();
    }
    
    //METODO TO STRING
    public String toString(){
        StringBuffer sb = new StringBuffer();
        sb.append("\t").append(this.nome).append("\n");
        sb.append("\tValor base para encomendas pequenas: ").append(String.valueOf(this.valorbasePequeno)).append("\n");
        sb.append("\tValor base para encomendas medias: ").append(String.valueOf(this.valorbaseMedio)).append("\n");
        sb.append("\tValor base para encomendas grande: ").append(String.valueOf(this.valorbaseGrande)).append("\n");
        sb.append("\tValor do imposto cobrado: ").append(String.valueOf(this.imposto)).append("\n");
        sb.append("\tValor total faturado pela transportadora: ").append(String.valueOf(this.precostotais)).append("\n");
        sb.append("\tNumero de encomendas ao servico da loja: ").append(String.valueOf(this.numencs)).append("\n");
        return sb.toString();
    }
    
}
