package ard;

import java.io.Reader;

public class RecursiveDescending extends Ard {

	protected RecursiveDescending(Reader in) {
		super(in);
	}

	protected boolean isChiffre() {
		return Character.isDigit(this.current);
	}

	protected boolean isAlphabet() {
		return ((this.current == 'a') || (this.current == 'b') || (this.current == 'c'));
	}

	protected void E() throws SyntaxException, ParserException {
		if (this.isAlphabet()) { // E -> L
			this.next();
		} else {
			switch (this.current) {
			case '(': // E->(S)
				this.next();
				this.S();
				eat(')');
				break;
			default: // Error
				throw new SyntaxException(ErrorType.NO_RULE, current);
			}
		}
	}

	protected void S() throws SyntaxException, ParserException {
		if (this.isAlphabet() || this.current == '(') {// S-> ERS
			this.E();
			this.R();
			this.S();
		} else {
			switch (this.current) {
			case ')':
			case END_MARKER:
				break;
			default:
				throw new SyntaxException(ErrorType.NO_RULE, current);
			}
		}
	}

	protected void R() throws SyntaxException, ParserException {
		if (this.isChiffre()) { // R -> C
			this.next();
		} else {
			switch (this.current) {
			case 'a':
			case 'b':
			case 'c':
			case '(':
			case ')':
			case END_MARKER:
				break;
			default:
				throw new SyntaxException(ErrorType.NO_RULE, current);
			}
		}
	}

	protected void axiom() throws SyntaxException, ParserException {
		S();
	}
}
