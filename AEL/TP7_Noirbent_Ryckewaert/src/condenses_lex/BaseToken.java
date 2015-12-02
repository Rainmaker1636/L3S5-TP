package condenses_lex;
/**
 *   Minimal  YYtoken implementation
 **/

public class BaseToken implements Yytoken{
    private final TokenType type;
    
    /**
     * Token type
     */
    public TokenType getType(){
        return type;
    }
    protected BaseToken(TokenType type){
        this.type = type;
    }
    public String toString(){
        return "<"+type+">";
    }
    
    public boolean isChiffre(){
    	return false;
    }
    
    public boolean isAlphabet(){
    	return false;
    }
    
	public boolean isOuvrante() {
		return false;
	}
}