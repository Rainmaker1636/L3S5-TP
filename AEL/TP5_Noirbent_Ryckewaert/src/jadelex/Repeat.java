package jadelex;

/**
 *  YYtoken implementation for Length of repeats
 */

public class Repeat extends BaseToken{
    private int times;
    
    public Repeat(int times){
        super(TokenType.REPEAT);
        this.times=times;
    }
    
    public int getOccurrences(){
	return this.times;
    }	
    
    public String toString(){
        return "<REPEAT>["+times+"]";
    }
}
