package Ficha3.Ex1;

import java.util.Arrays;

public class Telemovel{
    
    private String marca;
    private String modelo;
    private int displayX;
    private int displayY;
    
    private int nMensagens;
    private String[] mensagens;
    private int armazenaTotal;
    private int armazenaFotos;
    private int armazenaApps;
    private int espacoTotal;
    
    private int nFotos;
    private int nApps;
    private String[] apps;
    
    public Telemovel(String marca, String modelo, int dX, int dY, int armT, int armFotos, int armApps, int mens){
        this.marca = new String(marca);
        this.modelo = new String(modelo);
        this.displayX = dX;
        this.displayY = dY;
        
        this.nMensagens = mens;
        this.armazenaTotal = armT;
        this.armazenaFotos = armFotos;
        this.armazenaApps = armApps;
        this.espacoTotal = 0;
        this.nFotos = 0;
        this.nApps = 0;
        this.apps = new String[armApps];
        this.mensagens = new String[mens];
    }
    
    public Telemovel(){
        this("Não declarada", "Não declarado", 100, 100, 64, 32, 32, 10);
    }
    
    public String toString(){
        return "Marca: " + this.getMarca() +
                "\nModelo: " + this.getModelo() +
                "\nx Display: " + this.getDisplayX() +
                "\ny Display: " + this.getDisplayY() +
                "\nNumero de Mensagens: " + this.getNMensagens() +
                "\nArmazenamento: " + this.getArmazenaT() +
                "\nArmazenamento de fotos: " + this.getArmazenaFotos() +
                "\nArmazenamento de apps: " + this.getArmazenaApps() +
                "\nEspaço Ocupado: " + this.getEspacoTotal() +
                "\nNumero de fotos: " + this.getNFotos() +
                "\nNumero de Apps: " + this.getNApps();
    }
    
    public boolean equals(Telemovel o){
        if(o == this) return true;
        
        if(o == null || o.getClass() != this.getClass()) return false;
        
        return this.getMarca().equals(o.getMarca()) && this.getModelo().equals(o.getModelo()) &&
                this.getDisplayX() == o.getDisplayX() && this.getDisplayY() == o.getDisplayY() &&
                this.nMensagens == o.nMensagens && this.armazenaTotal == o.armazenaTotal &&
                this.armazenaApps == o.armazenaApps && this.armazenaFotos == o.armazenaFotos &&
                this.espacoTotal== o.espacoTotal && this.nFotos == o.nFotos && this.nApps == o.nApps;
    }
    
    public boolean existeEspaco(int numeroBytes){
        return this.espacoTotal > numeroBytes;
    }
    
    public void instalaApp(String nome, int tamanho){
        if(existeEspaco(tamanho) && tamanho <= this.getArmazenaApps()){
            int n = this.getNApps();
            this.apps[n++] = new String(nome);
            this.setNApps(n);
            this.setEspacoTotal(this.getEspacoTotal()+tamanho);
            this.setArmazenaApps(this.getArmazenaApps() + tamanho);
        }
    }
    
    public void recebeMsg(String msg){
        int n = this.getNMensagens() + 1;
        String[] novo = this.getMensagens();
        this.mensagens = new String[n];
        System.arraycopy(novo, 0, this.mensagens, 0, n-1);
        this.mensagens[n-1] = new String(msg);
        this.setNMensagens(n);
    }
    
    public double tamMedioApps(){
        double media = getArmazenaApps();
        media = media / (double) this.getNApps();
        return media;
    }
    
    public String maiorMsg(){
        String[] msg = this.getMensagens();
        
        if(this.getNMensagens() < 1)
            return null;
        
        String maiorMsg = msg[0];
        int maior = maiorMsg.length();
        
        for(int i = 1; i < this.getNMensagens(); i++){
            if(msg[i].length() > maior){
                maiorMsg = msg[i];
                maior = maiorMsg.length();
            }
        }
        return maiorMsg;
    }
    
    public void removeApp(String nome, int tamanho){
        int n = this.getNApps();
        String[] app = this.getApps();
        this.mensagens = new String[n-1];
        boolean flag = false;
        for(int i = 0, p = 0; i < n; i++){
            if(app[i].equals(nome)){
                flag = true;
            }
            else
                this.mensagens[p++] = app[i];
        }
        if(flag){
            this.setNApps(n-1);
            this.setArmazenaApps(this.getArmazenaApps() - tamanho);
            this.setEspacoTotal(this.getEspacoTotal() - tamanho);
        }
    }
    
    
    // Sets e Gets //
    
    public String getMarca(){
        return this.marca;
    }
    
    public String getModelo(){
        return this.modelo;
    }
    
    public int getDisplayX(){
        return this.displayX;
    }
    
    public int getDisplayY(){
        return this.displayY;
    }
    
    public int getNMensagens(){
        return this.nMensagens;
    }
    
    public int getArmazenaT(){
        return this.armazenaTotal;
    }
    
    public int getArmazenaFotos(){
        return this.armazenaFotos;
    }
    
    public int getArmazenaApps(){
        return this.armazenaApps;
    }
    
    public int getEspacoTotal(){
        return this.espacoTotal;
    }
    
    public int getNFotos(){
        return this.nFotos;
    }
    
    public int getNApps(){
        return this.nApps;
    }
    
    public String[] getMensagens(){
        String[] novo = new String[this.nMensagens];
        
        if(this.nMensagens >= 0)
            System.arraycopy(this.mensagens, 0, novo, 0, this.nMensagens);
        return novo;
    }
    
    public String[] getApps(){
        String[] novo = new String[this.nApps];
        
        if(this.nApps > 0)
            System.arraycopy(this.mensagens, 0, novo, 0, this.nApps);
        return novo;
    }
    
    
    public void setMarca(String newMarca){
        this.marca = new String(newMarca);
    }
    
    public void setModelo(String newModelo){
        this.modelo = new String(newModelo);
    }
    
    public void setDisplayX(int newDisplayX){
        this.displayX = newDisplayX;
    }
    
    public void setDisplayY(int newDisplayY){
        this.displayY = newDisplayY;
    }
    
    public void setNMensagens(int newNMensagens){
        this.nMensagens = newNMensagens;
    }
    
    public void setArmazenaT(int newArmazenaTotal){
        this.armazenaTotal = newArmazenaTotal;
    }
    
    public void setArmazenaFotos(int newArmazenaFotos){
        this.armazenaFotos = newArmazenaFotos;
    }
    
    public void setArmazenaApps(int newArmazenaApps){
        this.armazenaApps = newArmazenaApps;
    }
    
    public void setEspacoTotal(int newEspacoTotal){
        this.espacoTotal = newEspacoTotal;
    }
    
    public void setNFotos(int newNFotos){
        this.nFotos = newNFotos;
    }
    
    public void setNApps(int newNApps){
        this.nApps = newNApps;
    }
}

