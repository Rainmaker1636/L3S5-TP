package ard;

import java.io.Reader;
import java.util.ArrayList;

import condenses_lex.*;

public class RecursiveDescending extends Ard {

	public static final Eod END_MARKER = new Eod("#");
	protected Yytoken current;
	protected ArrayList<Yytoken> tokens;
	protected int currentToken;
	protected Tokenizer yy;

	protected RecursiveDescending(Reader in) {
		super(in);
		yy = new TokenizerV1(in);
		Yytoken token;
		tokens = new ArrayList<Yytoken>();
		try {
			while ((token = yy.yylex()) != null) {
				tokens.add(token);
			}
		} catch (Exception e) {
			System.out.println("Aucun token ajouté");
		}
		this.currentToken = 0;
		this.current = this.tokens.get(currentToken);
	}

	protected void next() throws ParserException {
		if (current.getType() == END_MARKER.getType()) {
			throw new ParserException();
		}
		shift();
	}

	@Override
	protected void shift() {
		if (yy != null) {
			try {
				this.currentToken++;
				this.current = this.tokens.get(currentToken);
			} catch (Exception e) {
				current = END_MARKER;
			}
		}
	}

	/**
	 * Vérifie que le caractère courant correspond au caractère attendu puis
	 * progresse d'une position dans la lecture du texte.
	 * 
	 * @throws SyntaxException
	 *             si la vérification de correspondance échoue
	 */
	protected void eat(TokenType expected) throws SyntaxException, ParserException {
		if (current.getType() != expected)
			// throw new ParserException("expected : " + c + ", found : " +
			// current);
			throw new SyntaxException(ErrorType.UNMATCHING_CHAR, current, expected);
		next();
	}

	/**
	 * Analyse le texte. Déclenche une exception en cas d'erreur.
	 * 
	 * Ne peut être invoquée qu'une seule fois.
	 * 
	 * @throws SyntaxException
	 *             En cas d'erreur de syntaxe
	 * @throws ParserException
	 *             En cas d'erruer de utilisation du parser.
	 */
	public void parse() throws SyntaxException, ParserException {
		if (invalid)
			throw new ParserException();
		// Axiom :
		axiom();
		// check end of data :
		if (current != END_MARKER)
			throw new SyntaxException(ErrorType.UNMATCHING_CHAR, current);
		this.invalid = true;
	}

	protected Entier R() throws SyntaxException, ParserException {
		if (this.current.isChiffre()) { // R -> C
			Entier nb=(Entier) this.current;
			this.next();
			return nb;
		} else {
			return new Entier(1);
		}
	}

	protected String E() throws SyntaxException, ParserException {
		if (this.current.isAlphabet()) { // E -> L
		String mot=((Lettre)this.current).getValue();
		this.next();
		return mot;
		} else {
			switch (this.current.getType()) {
			case OUVRANTE: // E->(S)
				this.next();
				String mot = S();
				eat(TokenType.FERMANTE);
				return mot;
			default: // Error
				throw new SyntaxException(ErrorType.NO_RULE, current);
			}
		}
	}

	protected String S() throws SyntaxException, ParserException {
		String mot = "";
		if (this.current.isAlphabet() || this.current.isOuvrante()) {
			return this.repeat(E(), R()) + S();
		} else {
			switch (this.current.getType()) {
			case FERMANTE:
			case EOD:
				break;
			default:
				throw new SyntaxException(ErrorType.NO_RULE, current);
			}
		}
		return mot;
	}

	protected String repeat(String mot, Entier nb) {
		if (nb.getValue() == 1)
			return mot;
		else
			return (mot + this.repeat(mot, new Entier(nb.getValue() - 1)));
	}

	protected void axiom() throws SyntaxException, ParserException {
		System.out.println(S());
	}
}
