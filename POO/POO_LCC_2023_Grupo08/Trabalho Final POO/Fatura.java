import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.HashMap;

public class Fatura implements Serializable
{
    private int nif;
    private String nome;
    private String[] compras;
    private double[] precos;
    private int numcompras;
    private double precoantes;
    private double precototal;
    private double custostransp;
    private double imposto;
    private LocalDateTime horacompra;
    
    
    public Fatura(int nif, String nome, String[] compras, double[] precos, int numcompras, double precoantes, double custostransp, double imposto, LocalDateTime horacompra){
        this.nif=nif;
        this.nome=nome;
        this.compras=compras;
        this.precos=precos;
        this.numcompras=numcompras;
        this.precoantes=precoantes;
        this.imposto=imposto;
        this.precototal=precoantes+(precoantes*imposto)+custostransp;
        this.custostransp=custostransp;
        this.horacompra=horacompra;
    }
    
    //METODOS GET E SET
    public int getNif(){
        return this.nif;
    }
    public String getNome(){
        return this.nome;
    }
    public String[] getCompras(){
        return this.compras;
    }
    public double[] getPrecos(){
        return this.precos;
    }
    public int getNumCompras(){
        return this.numcompras;
    }
    public double getPrecoAntes(){
        return this.precoantes;
    }
    public double getPrecoTotal(){
        return this.precototal;
    }
    public double getCustosTransp(){
        return this.custostransp;
    }
    public double getImposto(){
        return this.imposto;
    }
    public LocalDateTime getHoraCompra(){
        return this.horacompra;
    }
    
    public void setNif(int nif){
        this.nif=nif;
    }
    public void setNome(String nome){
        this.nome=nome;
    }
    public void setCompras(String[] compras){
        this.compras=compras;
    }
    public void setPrecos(double[] precos){
        this.precos=precos;
    }
    public void setNumCompras(int numcompras){
        this.numcompras=numcompras;
    }
    public void setPrecoAntes(double precoantes){
        this.precoantes=precoantes;
    }
    public void setPrecoTotal(double precototal){
        this.precototal=precototal;
    }
    public void setCustosTransp(double custostransp){
        this.custostransp=custostransp;
    }
    public void setImposto(double imposto){
        this.imposto=imposto;
    }
    public void setHoraCompra(LocalDateTime horacompra){
        this.horacompra=horacompra;
    }
    
    //METODO EQUALS
    public boolean equals(Object o){
        if(this==o) return true;
        if(o==null || o.getClass()!=this.getClass()) return false;
        Fatura f = (Fatura) o;
        return this.nif==f.getNif() && this.horacompra.equals(f.getHoraCompra());
    }
    
    //METODO TOSTRING
    public String toString(){
        StringBuffer sb = new StringBuffer();
        sb.append("\tVINTAGE\n");
        sb.append("\tHORA DA COMPRA: ").append(this.horacompra.toString()).append("\n");
        sb.append("\tNIF: ").append(String.valueOf(this.nif)).append("\n");
        sb.append("\tNOME: ").append(this.nome).append("\n");
        sb.append("\tPRODUTO ------- PRECO\n");
        for(int i=0; i<this.numcompras; i++){
            sb.append("\t").append(this.compras[i]).append("-----").append(String.valueOf(this.precos[i])).append("\n");
        }
        sb.append("\tVALOR: ").append(String.valueOf(this.precoantes)).append("\n");
        sb.append("\tIMPOSTO: ").append(String.valueOf(this.imposto)).append("\n");
        sb.append("\t---------------------------------------------------\n");
        sb.append("\tCUSTOS ADICIONAIS DE TRANSPORTE: ").append(String.valueOf(this.custostransp)).append("\n");
        sb.append("\t---------------------------------------------------\n");
        sb.append("\tTOTAL: ").append(String.valueOf(this.precototal)).append("\n");
        return sb.toString();
    }
    
    
    
}