package jadelex;

/**
 *  YYtoken implementation for Length of movement
 */

public class StepLength extends BaseToken{
    private int length;
    
    public StepLength(int length){
        super(TokenType.PEN_MODE);
        this.length=length;
    }

	public int getLength(){
		return this.length;
	}	

    public String toString(){
        return "<LENGHT>["+length+"]";
    }
}
