import java.util.ArrayList;
import java.util.HashMap;
import java.io.Serializable;
import java.io.ObjectOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.FileInputStream;
import java.io.StreamCorruptedException;
import java.util.Collection;
import java.util.List;
import java.util.Random;

public class Vintage implements Serializable
{
    private HashMap<Integer, Utilizador> users;
    private ArrayList<Transportador> transportadores;
    private ArrayList<String> codigos;
    private double impostotransp;
    private double impostovenda;

    public Vintage(){
        HashMap<Integer, Utilizador> users = new HashMap<Integer, Utilizador>();
        ArrayList<Transportador> transportadores = new ArrayList<Transportador>();
        ArrayList<String> codigos = new ArrayList<String>();
        this.users=users;
        this.transportadores=transportadores;
        this.codigos=codigos;
    }
    public Vintage(double impostotransp, double impostovenda){
        HashMap<Integer, Utilizador> users = new HashMap<Integer, Utilizador>();
        ArrayList<Transportador> transportadores = new ArrayList<Transportador>();
        ArrayList<String> codigos = new ArrayList<String>();
        this.users=users;
        this.transportadores=transportadores;
        this.codigos=codigos;
        this.impostovenda=impostovenda;
        this.impostotransp=impostotransp;
    }
    
    
    
    //SETS E GETS
    public HashMap<Integer, Utilizador> getUsers(){
        return this.users;
    }
    public ArrayList<Transportador> getTransportadores(){
        return this.transportadores;
    }
    public ArrayList<String> getCodigos(){
        return this.codigos;
    }
    public double getImpostoTransp(){
        return this.impostotransp;
    }
    public double getImpostoVenda(){
        return this.impostovenda;
    }
    
    
    public void setUsers(HashMap<Integer, Utilizador> users){
        this.users=users;
    }
    public void setTransportadores(ArrayList<Transportador> transportadores){
        this.transportadores=transportadores;
    }
    public void setCodigos(ArrayList<String> codigos){
        this.codigos=codigos;
    }
    public void setImpostoTransp(double impostotransp){
        this.impostotransp=impostotransp;
    }
    public void setImpostoVenda(double impostovenda){
        this.impostovenda=impostovenda;
    }
    
    //OUTROS METODOS
    public void addUser(Utilizador x)throws UtilizadorJaExisteException{
        if(this.users.containsKey(x.getNif())==false)this.users.put(x.getNif(), x);
        else{
            throw new UtilizadorJaExisteException("Utilizador ja registado");
        }
    }
    public void removeUser(Utilizador x){
        this.users.remove(x.getNif(), x);
        for(Transportador t : this.transportadores){
            for(Encomenda e : t.getEncomendas()){
                if(x.getNif()==e.getNifComprador()){
                    if(e.getEstado().equals("pendente")){
                        t.removeEncomenda(e);
                    }
                }
                if(e.getEstado().equals("pendente")){
                    for(Artigo a : e.getArtigos()){
                        if(this.quemPertence(a).equals(x)){
                            e.removeArtigo(a);
                        }
                    }
                }
            }
        }
    }
    
    public void addTransportador(Transportador x){
        this.transportadores.add(x);
    }
    public void removeTransportador(Transportador x){
        this.transportadores.remove(x);
    }
    
    public void addCodigo(String cod){
        this.codigos.add(cod);
    }
    
    public boolean existeUser(int nif){
        return this.users.containsKey(nif);
    }
    
    public boolean jaExisteEmail(String mail){
        boolean r = false;
        for(Utilizador u : this.todosUsers()){
            if(u.getEmail().equals(mail)){
                r=true;
            }
        }
        return r;
    }
    
    public Utilizador procuraUser(int nif)throws NaoExisteUtilizadorException{
        if(existeUser(nif)){ return this.users.get(nif);}
        else{throw new NaoExisteUtilizadorException("Utilizador Nao Existe");}
    }
    
    public boolean existeCodigo(String cod){
        boolean r = false;
        for(Utilizador u : this.todosUsers()){
            if(u.getCodigo().equals(cod)){
                r=true;
            }
        }
        return r;
    }
    
    public Utilizador procuraUserPeloCodigo(String cod){
        for(Utilizador u : this.todosUsers()){
            if(u.getCodigo().equals(cod)){
                return u;
            }
        }
        return null;
    }
    
    
    public Collection<Utilizador> todosUsers(){
        return this.users.values();
    }
    
    //UTILIZADOR QUE MAIS VENDEU EM DINHEIRO
    public Utilizador userMaisVendas() throws NaoExisteUtilizadorException {
        double preco=0;
        int nif = -1;
        for(Utilizador u : this.todosUsers()){
            if(u.getPrecoVendas()>=preco){
                nif=u.getNif();
                preco=u.getPrecoVendas();
            }
        }
        return this.procuraUser(nif);
    }
    
    public double precoMaisVendas(){
        double preco=0;
        for(Utilizador u : this.todosUsers()){
            if(u.getPrecoVendas()>=preco){
                preco=u.getPrecoVendas();
            }
        }
        return preco;
    }
    
    //UTILIZADOR QUE MAIS GASTOU A COMPRAR
    public Utilizador userMaisCompras(){
        double r=0;
        Utilizador ut = null;
        for(Utilizador u : this.todosUsers()){
            if(u.quantoGastou()>=r){
                r=u.quantoGastou();
                ut=u;
            }
        }
        return ut;
    }
    
    //TRABSPORTADORA QUE MAIS FATUROU
    public Transportador transpMaisFatura(){
        double r=0;
        Transportador t2 = null;
        for(Transportador t : this.transportadores){
            if(t.getPrecosTotais()>=r){
                r=t.getPrecosTotais();
                t2=t;
            }
        }
        return t2;
    }
    
    //CALCULA QUANTO A VINTAGE FATUROU
    public double valorFaturado(){
        double r=0;
        for(Utilizador u : this.todosUsers()){
            for(Fatura f : u.getFaturas()){
                r+=f.getPrecoAntes()*f.getImposto();
            }
        }
        for(Transportador t : this.transportadores){
            for(Encomenda e : t.getEncomendas()){
                if(e.getEstado().equals("entregue") && e.getTamanho().equals("pequena")){
                    
                        r+=t.getValorBasePequeno()*t.getImposto();
                    
                }
                else if(e.getEstado().equals("entregue") && e.getTamanho().equals("media")){ 
                    
                        r+=t.getValorBaseMedio()*t.getImposto();
                    
                }
                else if(e.getEstado().equals("entregue") && e.getTamanho().equals("grande")){
                    
                        r+=t.getValorBaseGrande()*t.getImposto();
                    
                }
            }
        }
        return r;
    }
    
    public boolean vazia(){
        return this.users.isEmpty();
    }
    
    public boolean existeTransportador(String nome){
        boolean r = false;
        for(Transportador t : this.transportadores){
            if(t.getNome().equals(nome)){
                r=true;
                break;
            }
        }
        return r;
    }
    
    public Transportador procuraTransportador(String nome)throws NaoExisteTransportadorException{
        if(existeTransportador(nome)){
            for(Transportador t : this.transportadores){
                if(t.getNome().equals(nome)){
                    return t;
                }
            }
            return null;
        }
        
        else{
            throw new NaoExisteTransportadorException("transportador nao associado a loja");
        }
    }
    
    
    
    public ArrayList<Encomenda> encomendasCompradas(Utilizador u){
        ArrayList<Encomenda> encs = new ArrayList<Encomenda>();
        for(Transportador t : this.transportadores){
            for(Encomenda e : t.getEncomendas()){
                if(e.getNifComprador()==u.getNif()){
                    encs.add(e);
                }
            }
        }
        return encs;
    }
    
    //dado um artigo, retorna o user que o vendeu, dado o seu codigo
    public Utilizador quemPertence(Artigo a){
        for(Utilizador u : this.todosUsers()){
            for(String cod : u.getCodigosArt()){
                if(a.getCodigo().equals(cod)){
                    return u;
                }
            }
        }
        return null;
    }
    
    //VE SE UM ARTIGO DE UMA ENCOMENDA PENDENTE FOI REMOVIDO DO SISTEMA
    public boolean foiremovido(Artigo art){
        if(this.quemPertence(art)!=null){
            boolean r=true;
            for(Artigo a : this.quemPertence(art).todosArtigos()){
                if(a.getCodigo()==art.getCodigo()){
                    r=false;
                }
            }
            return r;
        }
        return true;
    }
    
    public void encomendaAdicionaArtigo(Encomenda e, Artigo a){
        e.addArtigo(a);
        this.quemPertence(a).avender(a);
    }
    
    public void encomendaFinaliza(Encomenda e){
        for(Artigo a : e.getArtigos()){
            this.quemPertence(a).aumentaPrecoVendas(a);
        }
        e.finalizada();
    }
    
    
    public ArrayList<Artigo> artigosVendidos(Utilizador u){
        ArrayList<Artigo> arts = new ArrayList<Artigo>();
        for(String cod : u.getCodigosArt()){
            for(Transportador t : this.transportadores){
                for(Encomenda e : t.getEncomendas()){
                    if(e.getEstado().equals("entregue") || e.getEstado().equals("expedida")){
                        for(Artigo art : e.getArtigos()){
                            if(cod.equals(art.getCodigo())){
                                arts.add(art);
                            }
                        }
                    }
                }
            }
        }
        return arts;
    }
    
    public ArrayList<Artigo> artigosComprados(Utilizador u){
        ArrayList<Artigo> arts = new ArrayList<Artigo>();
        for(Transportador t : this.transportadores){
            for(Encomenda e : t.getEncomendas()){
                if(e.getEstado().equals("expedida") || e.getEstado().equals("entregue")){
                    for(Artigo a : e.getArtigos()){
                        arts.add(a);
                    }
                }
            }
        }
        return arts;
    }
    
    
   
    //FAZ A ENCOMENDA CHEGAR E A TRANSPORTADORA GANHAR DINHEIRO, E PASSA A FATURA PARA O COMPRADOR
    public void transpEntregaEnc(Transportador t, Encomenda e) throws NaoExisteUtilizadorException {
        t.encomendaChegou(e);
        
        String[] compras = new String[e.getNumArtigos()];
        double[] precos = new double[e.getNumArtigos()];
        double precoantes=0;
        int i=0;
        for(Artigo a : e.getArtigos()){
            compras[i]=a.getClass().getName().toString();
            precos[i]=a.getPrecoCorrecao();
            precoantes+=a.getPrecoCorrecao();
            i++;
            this.quemPertence(a).retiraAvender(a);
        }
        Fatura f = new Fatura(e.getNifComprador(), this.procuraUser(e.getNifComprador()).getNome(), compras, precos, e.getNumArtigos(), precoantes, t.precoExpedicao(e),
        this.impostovenda, e.getDataFin());
        
        this.procuraUser(e.getNifComprador()).addFatura(f);
    }
    
        
    public ArrayList<Utilizador> vendedores(Encomenda e){
        ArrayList<Utilizador> us = new ArrayList<Utilizador>();
        for(Artigo a : e.getArtigos()){
            us.add(this.quemPertence(a));
        }
        return us;
    }
    
    public String geraCodigo(){
        String caraters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
        StringBuffer codigo = new StringBuffer(10);
        
        Random random = new Random();
        
        for(int i=0 ; i<10; i++){
            int index= random.nextInt(caraters.length());
            char randomchar = caraters.charAt(index);
            codigo.append(randomchar);
        }
        String cod = codigo.toString();
        boolean existe = false;
        for(String s : this.codigos){
            if(s.equals(cod)==true){
                existe=true;
            }
        }
        if(existe==true){geraCodigo();}
    
        this.codigos.add(cod);
        return cod;
    }
    
    public ArrayList<Artigo> associados(Transportador t)  {
        ArrayList<Artigo> arts = new ArrayList<Artigo>();
        for(Utilizador u : this.todosUsers()){
            for(Artigo art : u.todosArtigos()){
                if(u.procuraTransp(art).equals(t)){
                    arts.add(art);
                }
            }
        }
        return arts;
    }
    
    
    
    
    
    //LER E ESCREVER VINTAGE
    public void gravaobj() throws IOException{
        FileOutputStream file = new FileOutputStream("data.obj");
        ObjectOutputStream oos = new ObjectOutputStream(file);
        
        oos.writeObject(this);
        oos.close();
    }
    
    public static Vintage leobj() throws ClassNotFoundException, IOException, StreamCorruptedException{
        FileInputStream file = new FileInputStream("data.obj");
        ObjectInputStream ois = new ObjectInputStream(file);
        
        Vintage vin = (Vintage) ois.readObject();
        ois.close();
        
        return vin;
    }
    
    
   
    
    
 
}