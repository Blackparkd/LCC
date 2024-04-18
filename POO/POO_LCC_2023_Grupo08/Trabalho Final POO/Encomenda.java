import java.util.ArrayList;
import java.time.LocalDateTime;
import java.io.Serializable;
import java.io.ObjectOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.FileInputStream;


public class Encomenda implements Serializable
{
    private final static String pequena="pequena";
    private final static String media="media";
    private final static String grande="grande";
    
    private final static String pendente="pendente";
    private final static String finalizada="finalizada";
    private final static String expedida="expedida";
    private final static String entregue="entregue";
    
   
    private ArrayList<Artigo> artigos;   
    private int nifcomprador; 
    private String morada;
    private int numartigos;    
    private double precofinal;
    private String tamanho;
    private String estado;
    private LocalDateTime datafin;
    private LocalDateTime dataexp;
    private LocalDateTime datachegada;
    
    public Encomenda(int nifcomprador, String morada){
        this.nifcomprador=nifcomprador;
        this.morada=morada;
        this.artigos=new ArrayList<Artigo>();
        this.numartigos=0;
        this.estado="pendente";
    }
    
    public void addArtigo(Artigo x){
        if(this.estado.equals("pendente")){
            this.artigos.add(x);
            this.numartigos++;
            this.precofinal+=x.getPrecoCorrecao();
            if(this.numartigos==1)this.tamanho="pequeno";
            if(this.numartigos>1 && this.numartigos<6) this.tamanho="medio";
            if(this.numartigos>5) this.tamanho="grande";
        }
    }
    
    
    
    public void removeArtigo(Artigo x){
        if(this.estado.equals("pendente")){
            this.artigos.remove(x);
            this.precofinal-=x.getPrecoCorrecao();
            this.numartigos--;
            if(this.numartigos<=1)this.tamanho="pequeno";
            if(this.numartigos>1 && this.numartigos<6) this.tamanho="medio";
            if(this.numartigos>5) this.tamanho="grande";
            }
    }
    
    public void finalizada(){
         if(this.estado.equals("pendente")){   
            this.estado="finalizada";
            this.datafin=LocalDateTime.now();
        }
    }
    
    public void expedida(){
        if(this.estado.equals("finalizada")){
            this.estado="expedida";
            this.dataexp=LocalDateTime.now();
        }                  
    }
   
   
    //METODOS DE ENTREGA
    public void entregue(){
        if(this.estado.equals("expedida")){
            this.estado="entregue";
            this.datachegada = LocalDateTime.now();
        }
    }
    
    
    
    //outros metodos
    public boolean temArtigo(Artigo x){
        boolean r=false;
        for(Artigo art : this.artigos){
            if(art.equals(x)){
                r=true;
            }
        }
        return r;
    }
    

    
    
   
    
    
    
    //METODOS GET
    public ArrayList<Artigo> getArtigos(){
        return this.artigos;
    }
    public int getNifComprador(){
        return this.nifcomprador;
    }
    public String getMorada(){
        return this.morada;
    }
    public int getNumArtigos(){
        return this.numartigos;
    }
    public String getTamanho(){
        return this.tamanho;
    }
    public String getEstado(){
        return this.estado;
    }
    public double getPrecoFinal(){
        return this.precofinal;
    }
    public LocalDateTime getDataFin(){
        return this.datafin;
    }
    public LocalDateTime getDataExp(){
        return this.dataexp;
    }
    public LocalDateTime getDataChegada(){
        return this.datachegada;
    }
    
    //METODOS SET
    public void setArtigos(ArrayList<Artigo> artigos){
        this.artigos=artigos;
    }
    public void setNifComprador(int nifcomprador){
        this.nifcomprador=nifcomprador;
    }
    public void setMorada(String morada){
        this.morada=morada;
    }
    public void setNumArtigos(int numartigos){
        this.numartigos=numartigos;
    }
    public void setTamanho(String tamanho){
        this.tamanho=tamanho;
    }
    public void setEstado(String estado){
        this.estado=estado;
    }
    public void setPrecoFinal(double precofinal){
        this.precofinal=precofinal;
    }
    public void setDataFin(LocalDateTime datafin){
        this.datafin=datafin;
    }
    public void setDataExp(LocalDateTime dataexp){
        this.dataexp=dataexp;
    }
    public void setDataChegada(LocalDateTime datachegada){
        this.datachegada=datachegada;
    }
    
    //TO String
    public String toString(){
        final StringBuffer sb = new StringBuffer();
        sb.append("\tMorada Destinataria= ").append(this.morada).append("\n");
        sb.append("\tNif Destinatario= ").append(String.valueOf(this.nifcomprador)).append("\n");
        sb.append("\tEstado= ").append(this.estado).append("\n");
        sb.append("\tNumero de Artigos= ").append(String.valueOf(this.numartigos)).append("\n");
        for(Artigo art : this.artigos){
            sb.append("\t").append(art.getClass().getName().toString()).append("\n");
        }
        if(this.getEstado().equals("finalizada")){
            sb.append("\tFinalizacao da encomenda: ").append(this.datafin.toString()).append("\n");
        }
        else if(this.getEstado().equals("expedida")){
            sb.append("\tFinalizacao da encomenda: ").append(this.datafin.toString()).append("\n");
            sb.append("\tExpedicao da encomenda: ").append(this.dataexp.toString()).append("\n");
        }
        else if(this.getEstado().equals("entregue")){
            sb.append("\tFinalizacao da encomenda: ").append(this.datafin.toString()).append("\n");
            sb.append("\tExpedicao da encomenda: ").append(this.dataexp.toString()).append("\n");
            sb.append("\tEntrega da encomenda: ").append(this.datachegada.toString()).append("\n");
        }
        
        return sb.toString();
    }
    
  
    
    //EQUALS
    public boolean equals(Object o){
        if(o==null || o.getClass()!=this.getClass()) return false;
        if(this==o) return true;
        Encomenda e = (Encomenda) o;
        return this.numartigos==e.getNumArtigos() && this.nifcomprador==e.getNifComprador() && this.morada.equals(e.getMorada());
    }
    
   

    
    

}