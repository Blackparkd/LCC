package Ficha3.Ex1;

import java.lang.Math;

public class Circulo
{
    // variáveis de instância - substitua o exemplo abaixo pelo seu próprio
    private double x;
    private double y;
    private double raio;

    public Circulo(double newX,double newY, double newRaio)
    {
        this.x = newX;
        this.y = newY;
        this.raio = newRaio;
    }
    
    public Circulo(){
        this.x = this.y = 0;
        this.raio = 1;
    }
    
    public Circulo(Circulo c){
        this.x = c.getX();
        this.y = c.getY();
        this.raio = c.getRaio();
    }
    
    public Circulo clone(){
        return new Circulo(this);
    }
    
    public boolean equals(Object o){
        if(this == o)
            return true;
        if((o == null) || (this.getClass() != o.getClass()))
            return false;
            
        Circulo c = (Circulo) o;
        return (this.x == c.getX() && this.y == c.getY() && this.raio == c.getRaio());
    }
    
    public String toString(){
        return "x= "+ this.x + "\ny= "+ this.y +"\nRaio= "+ this.raio;
    }
    
    public double getX(){
        return this.x;
    }
    
    public double getY(){
        return this.y;
    }
    
    public double getRaio(){
        return this.raio;
    }
    
    public void setX(double newX){
        this.x = newX;
    }
    
    public void setY(double newY){
        this.y = newY;
    }
    
    public void setRaio(double newRaio){
        this.raio = newRaio;
    }
    
    public void alteraCentro(double newX, double newY){
        this.x = newX;
        this.y = newY;
    }
    
    public double calculaArea(){
        return Math.PI * this.raio * this.raio;
    }
    
    public double calculaPerimetro(){
        return Math.PI * 2 * this.raio;
    }
    
    
    
    
}
