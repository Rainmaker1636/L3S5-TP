package jade;
import java.awt.Point;
import jade.*;
import drawing.*;

public class DrawingMachine implements JadeMachine {
    private int pas;
    private boolean write;
    private Drawing draw;
    private int x;
    private int y;
    
    public DrawingMachine(){
	this.pas=5;
	this.draw= new DrawingFrame();
	this.x=0;
	this.y=0;
	draw.reset();
	this.write=false;
    }
    
    public void setStepLength(int n){
	this.pas=n;
    }
    
    /**
     * fixer le mode crayon (true => écriture)
     */
    public void setPenMode(boolean penMode){
	this.write=penMode;
    }
    
    /**
     * réaliser un mouvement élémentaire de la longueur d'un pas dans la direction indiquée
     * si le mode crayon est activé, le segment de droite est tracé.
     *
     */
    public void move(Direction d)  {
	try{
	    this.x += pas*d.getX();
	    this.y +=pas*d.getY();
	    if (write) {
		Point p = new Point(this.x,this.y);
		draw.drawTo(p);
	    }else {
		draw.goTo(new Point(this.x,this.y));
	    }
	    
        }catch( DrawingException e){System.out.println("hors limite pas pris en compte");}
    }
    
    /**
     * aller jusqu'au point de coordonnées indiquées, sans dessiner.
     *
     */
    public void jump(int x, int y) {
	try {
	    this.x=x;
	    this.y=y;
	    draw.goTo(new Point(this.x,this.y));
	}catch ( DrawingException e) {System.out.println("hors limite pas pris en compte");}
    }
    
    /**
     * stopper la machine. Toute modification de son état est ensuite impossible
     *
     */
    public void bye(int x, int y){
	
    }
}
