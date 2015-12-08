package condenses_lex;
/**
 *  Jade Tokenizer  result
 */
public interface Yytoken {

    TokenType getType();

	boolean isAlphabet();

	boolean isChiffre();

	boolean isOuvrante();
}