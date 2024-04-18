
import java.util.ArrayList;
import java.io.Serializable;
import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.FileInputStream;
import java.util.HashMap;
import java.util.Collection;
import java.util.Set;

public class Utilizador implements Serializable
{
    private String codigo;
    private String email;
    private String nome;
    private String morada;
    private int nif;
    private HashMap<Artigo, Transportador> avenda;
    private ArrayList<Artigo> avender;
    private ArrayList<String> codigosart;
    private double precovendas;
    private ArrayList<Fatura> faturas;
    
    public Utilizador(String codigo, String email, String nome, String morada, int nif){
        this.codigo=codigo;
        this.email=email;
        this.nome=nome;
        this.morada=morada;
        this.nif=nif;
        this.precovendas=0;
        this.codigosart= new ArrayList<String>();
        this.avenda = new HashMap<Artigo, Transportador>();
        this.avender= new ArrayList<Artigo>();
        this.faturas= new ArrayList<Fatura>();
    }
    
    public Artigo daArtigo(String codigo){
        for(Artigo art : this.todosArtigos()){
            if(codigo.equals(art.getCodigo())){
                return art;
            } 
        }
        return null;
    }
    
    
    public void avenda(Artigo x, Transportador t){
        this.avenda.put(x, t);
        this.codigosart.add(x.getCodigo());
    }
     public void retiravenda(Artigo x){
        this.avenda.remove(x, this.avenda.get(x));
    }
  
    
    public void avender(Artigo x){
        this.retiravenda(x);
        this.avender.add(x);
    }
    public void retiraAvender(Artigo x){
        this.avender.remove(x);
    }
    
    public Transportador procuraTransp(Artigo art){
        return this.avenda.get(art);
    }
    
    public Set<Artigo> todosArtigos(){
        return this.avenda.keySet();
    }
    
    public boolean meuartigo(Artigo x){
        boolean r=false;
        for(Artigo a : this.todosArtigos()){
            if(a.equals(x)){
                r=true;
            }
        }
        return r;
    }
    
  
    public void aumentaPrecoVendas(Artigo x){
        this.precovendas+=x.getPrecoCorrecao();
    }
    public void diminuiPrecoVendas(Artigo x){
        this.precovendas-=x.getPrecoCorrecao();
    }

    
    public void addFatura(Fatura f){
        this.faturas.add(f);
    }
    public void removefatura(Fatura f){
        this.faturas.remove(f);
    }
    //CALCULA QUANDO UM UTILIZADOR GASTOU EM COMPRAS BASEADO NAS SUAS FATURAS
    public double quantoGastou(){
        double r=0;
        for(Fatura f : this.getFaturas()){
            r+=f.getPrecoTotal();
        }
        return r;
    }
    
    
    
    //METODOS GET
    public String getCodigo(){
        return this.codigo;
    }
    public String getEmail(){
        return this.email;
    }
    public String getNome(){
       return this.nome;
    }
    public String getMorada(){
        return this.morada;
    }
    public int getNif(){
        return this.nif;
    }
    public double getPrecoVendas(){
        return this.precovendas;
    }

    public ArrayList<String> getCodigosArt(){
        return this.codigosart;
    }
    public ArrayList<Artigo> getAvender(){
        return this.avender;
    }
    public HashMap<Artigo, Transportador> getAvenda(){
        return this.avenda;
    }
    public ArrayList<Fatura> getFaturas(){
        return this.faturas;
    }
    
    //METODOS SET
    public void setCodigo(String codigo){
        this.codigo=codigo;
    }
    public void setEmail(String email){
        this.email=email;
    }
    public void setNome(String nome){
        this.nome=nome;
    }
    public void setMorada(String morada){
        this.morada=morada;
    }
    public void setNif(int nif){
        this.nif=nif;
    }
    public void setPrecoVendas(double precovendas){
        this.precovendas=precovendas;
    }
    public void setCodigosArt(ArrayList<String> codigosart){
        this.codigosart=codigosart;
    }
    public void setAvenda(HashMap<Artigo, Transportador> avenda){
        this.avenda=avenda;
    }
    public void setAvender(ArrayList<Artigo> avender){
        this.avender=avender;
    }
    public void setFaturas(ArrayList<Fatura> faturas){
        this.faturas=faturas;
    }
    
    //Metodo equals
    public boolean equals(Object o){
        if(o==null || this.getClass()!=o.getClass()) return false;
        if(this==o)return true;
        Utilizador user = (Utilizador) o;
        return user.getCodigo().equals(user.getCodigo());
    }
    
    //TO STRING
    public String toString(){
        
        final StringBuffer sb = new StringBuffer();
        sb.append("\tCodigo= ").append(this.codigo).append("\n");
        sb.append("\tEmail= ").append(this.email).append("\n");
        sb.append("\tNome= ").append(this.nome).append("\n");
        sb.append("\tMorada= ").append(this.morada).append("\n");
        sb.append("\tNif= ").append(String.valueOf(this.getNif())).append("\n");
        sb.append("\tPreco total das vendas= ").append(String.valueOf(this.getPrecoVendas())).append("\n");
      //  sb.append("\tPreco total das compras= ").append(String.valueOf(this.getprecocompras())).append("\n");
        return sb.toString();
    }
    

    
}