package jade;
import jadelex.*;

public class ParserLevel1 implements JadeParser {
    private final jadelex.Tokenizer tokenizer;
    private final jade.JadeMachine machine;
    
    public ParserLevel1(jadelex.Tokenizer tokenizer, jade.JadeMachine machine){
        this.tokenizer = tokenizer;
        this.machine = machine;
    }
    
    
    /*----------------------------------------------
     * Les méthodes ...Action reçoivent en paramètre un token
     * correspondant à une instruction simple et
     * envoient à la JadeMachine
     * la commande correspondante 
     *------------------------------------------------
     */
    
    /*
     * Déclenche l'action correspondant à un instruction simple
     */
    private void simpleAction(Yytoken t) {
	switch(t.getType()){
	case MOVE : 
	    moveAction((Move) t);
	    break;
	case PEN_MODE :
	    penModeAction((PenMode) t);
	    break;
	case JUMP : 
	    jumpAction((Jump) t);
	    break;
	case STEP_LENGTH :
	    stepLengthAction((StepLength) t);
	    break;
	}
    }

    /*
     * déclenche le déplacement indiqué par le token
     */
    private void moveAction(Move token){
	machine.move(token.getDirection());
    }
    
    /*
     * déclenche le changement de mode indiqué par le token
     */
    private void penModeAction(PenMode token){
	machine.setPenMode(token.getMode());
    }
    
    /*
     * déclenche le saut indiqué par le token
     */
    private void jumpAction(Jump token){
	machine.jump(token.getX(),token.getY());
    }
    
    /*
     * déclenche le changement de longueur de pas indiqué par le token
     */
    private void stepLengthAction(StepLength token){
	machine.setStepLength(token.getLength());
    }
    
    /*
     * Indique si le token correspond à une instruction simple
    */
    private boolean isSimple(Yytoken t){
	TokenType type;
	type=t.getType();
	return (type == TokenType.MOVE ||type == TokenType.PEN_MODE || type ==TokenType.JUMP || type == TokenType.STEP_LENGTH);
    }
    
    /*--------------------------------------
     * Analyseur syntaxique proprement dit
     *--------------------------------------
     */
    /*
     * dernier token lu
     */
    private jadelex.Yytoken currentToken;
    private jadelex.Yytoken previousToken;
    
    /*
     * progession de lecture. Modifie currentToken
     */
    private void nextToken() throws java.io.IOException{
        previousToken=currentToken;
	currentToken = tokenizer.yylex();
    }
    
    /*
     * Appelée en cas d'erreur de syntaxe
     */
    private void error() throws JadeException {  
	throw new JadeException();
    }

    /*
     * Cette méthode lit une séquence d'instructions terminée par la fin de fichier
     *
     * Initialement, currentToken est le premier token de la séquence à analyser
     * En fin de méthode, currentToken est le premier token QUI SUIT la séquence à analyser (donc ici null)
     *
     * 
     */
    private void parseSequence() throws java.io.IOException, JadeException {
	while (currentToken != null){
	    if (isSimple(currentToken)){
		simpleAction(currentToken);
		nextToken();
	    }
	    else{
		if (currentToken.getType()== TokenType.REPEAT){
		     parseRepeat();	
		}
		else
		    error();
	    }
	}
    }

    /*
     * Cette méthode lit une répétition complète, c'est à dire un opérateur de répétition
     * suivi d'une instruction simple à répéter
     * 
     * Initialement, currentToken est le premier token à analyser (donc de type REPEAT)
     * En fin de méthode, currentToken est le premier token QUI SUIT l'instruction à répéter
     *
     */
    private void parseRepeat() throws java.io.IOException, JadeException {
	Repeat repeat=(Repeat) currentToken;
	int fois=repeat.getOccurrences();
        for (int i=0;i<fois-1;i++){
	    if (isSimple(previousToken))
		simpleAction(previousToken);
	    else
		error();
	}
	nextToken();
    }
    
    /**
     * Déclenche l'analyse syntaxique et l'interprétation
     *
     */
    public void run() throws java.io.IOException, JadeException{
	// initialise puis lance l'analyse d'une séquence
	nextToken();
	parseSequence();
    }
 
}
