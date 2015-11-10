package jadelex;

import jade.Direction;
/**
 *  YYtoken implementation for jump
 */

public class Jump extends BaseToken{
    private int x;
    private int y;

    public Jump(int x,int y){	
        super(TokenType.JUMP);
        this.x=x;
        this.y=y;
    }
    
    public int getX(){
	return this.x;
    }	
    
    
    public int getY(){
	return this.y;
    }	
    
    public String toString(){
        return "<JUMP>["+this.getX()+";"+this.getY()+"]";
    }
}
