package jadelex;

import jade.Direction;
/**
 *  YYtoken implementation for Mouvement
 */

public class Move extends BaseToken{
    private Direction direction;
    
    public Move(Direction direction){
        super(TokenType.PEN_MODE);
        this.direction=direction;
    }

	public Direction getDirection(){
		return this.direction;
	}	

    public String toString(){
        return "<MOVE>["+direction+"]";
    }
}
