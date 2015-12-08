package condenses_lex;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

class Test {
  
  
  public static void main(String arg[]) throws IOException {
      // créer un Yylex qui va prendre ses entrées dans le fichier
      // de nom arg[0]
    Reader input;
    if (arg.length>0){
      input = new BufferedReader(new FileReader(arg[0]));
    }
    else {
      input = new InputStreamReader(System.in);
    }
    Tokenizer yy = new TokenizerV1(input) ;
    Yytoken token ;
    while ((token = yy.yylex()).getType() !=TokenType.EOD  ){
      //System.out.print("["+token.image()+"]<"+token.nom()+">");
      System.out.print(token+"\n");
    }
  }
}
