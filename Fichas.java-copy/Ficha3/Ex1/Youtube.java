package Ficha3.Ex1;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.time.temporal.ChronoUnit;

/**
 * Escreva uma descrição da classe Youtube aqui.
 * 
 * @author (seu nome) 
 * @version (um número da versão ou uma data)
 */
public class Youtube
{
    private String nomeVideo;
    private char[] video;
    private LocalDateTime data;
    private int resolucao;
    private int[] duracao;
    private String[] comentarios;
    
    private final int likes = 0;
    private final int dislikes = 1;
    private final int minutos = 0;
    private final int segundos = 1;
    private int[] reactions;
    
    public Youtube()
    {
        this.nomeVideo = "[Insert name here]";
        this.video = new char[0];
        this.data = LocalDateTime.now();
        this.resolucao = 1080;
        this.duracao = new int[2];
        this.duracao[minutos] = 0;
        this.duracao[segundos] = 0;
        this.comentarios = new String[0];
        this.reactions = new int[2];
        this.reactions[likes] = 0;
        this.reactions[dislikes] = 0;
    }
    public Youtube(String nome, char[] video, int resol, int min, int sec){
        this.nomeVideo = new String(nome);
        this.video = new char[video.length];
        System.arraycopy(video, 0, this.video, 0, video.length);
        this.resolucao = resol;
        this.duracao[minutos] = min;
        this.duracao[segundos] = sec;
        this.reactions[likes] = this.reactions[dislikes] = 0;
        this.comentarios = new String[0];
    }
    
    public boolean equals(Youtube o){
        if(this == o)
            return true;
        
        if(o == null || this.getClass() != o.getClass())
            return false;
        
            return this.nomeVideo.equals(o.nomeVideo) && Arrays.equals(this.video, o.video) &&
                    this.resolucao == o.resolucao && Arrays.equals(this.duracao, o.duracao) &&
                    Arrays.equals(this.reactions, o.reactions);
    }
    
    public String toString(){
        return "Video: " + this.getNomeVideo() + "\nDuraçao: " + this.getMinutos() + "minutos e " + this.getSegundos() + "segundos" +
                "\nLikes: " + this.getLikes() + "\nDislikes: "+ this.getDislikes();
    }
    
    // Sets e gets //
    
    public String getNomeVideo(){
        return this.nomeVideo;
    }
    
    public char[] getVideo(){
        char[] novo = new char[this.video.length];
        System.arraycopy(this.video, 0, novo, 0, this.video.length);
        return novo;
    }
    
    public int getResolucao(){
        return this.resolucao;
    }
    
    public int getMinutos(){
        return this.duracao[minutos];
    }
    
    public int getSegundos(){
        return this.duracao[segundos];
    }
    
    public String[] getComentarios(){
        String[] novo = new String[this.comentarios.length];
        System.arraycopy(this.comentarios, 0, novo, 0, this.comentarios.length);
        return novo;
    }
    
    public LocalDateTime getData(){
        return this.data;
    }
    
    public int getLikes(){
        return this.reactions[likes];
    }
    
    public int getDislikes(){
        return this.reactions[dislikes];
    }
    
    
    public void setNomeVideo(String nome){
        this.nomeVideo = new String(nome);
    }
    
    public void setVideo(char[] vid){
        this.video = new char[video.length];
        System.arraycopy(vid, 0, this.video, 0, video.length);
    }
    
    public void setResolucao(int re){
        this.resolucao = re;
    }
    
    public void setMinutos(int m){
        this.duracao[minutos] = m;
    }
    
    public void setSegundos(int s){
        this.duracao[segundos] = s;
    }
    
    public void setComentarios(String[] com){
        this.comentarios = new String[com.length];
        System.arraycopy(com, 0, this.comentarios, 0, com.length);
    }
    
    public void setLikes(int l){
        this.reactions[likes] = l;
    }
    
    public void setDislikes(int d){
        this.reactions[dislikes] = d;
    }
    
    
    // Perguntas Ficha
    
    public void insereComentario(String comentario){   
        String[] coms = this.getComentarios();
        String[] novo = new String[coms.length + 1];
        System.arraycopy(coms, 0, novo, 0, coms.length);
        novo[coms.length] = new String(comentario);
        this.setComentarios(novo);
    }
    
    public long qtsDiasDepois(){
        LocalDateTime upload = this.getData();
        LocalDateTime atual = LocalDateTime.now();
        
        return ChronoUnit.DAYS.between(upload, atual);
    }
    
    public void thumbsUp(){
        int like = this.getLikes();
        like++;
        this.setLikes(like);
    }
    
    public String processa(){
        StringBuilder s = new StringBuilder();
        char[] vid = this.getVideo();
        
        for(char c: vid)
            s.append(c);
        
        return s.toString();
    }
}






