package postfixees;

%%

%unicode
%line
%column
%{
  private int compteurCommentaire = 0 ;
%} 

%unicode

%state COMMENTAIRE

ENTIER_SIMPLE=[0-9]+
PLUS=[+]|plus
MINUS=[-]|minus
MULT=[*]|mult
QUO=[/]|quo
OPP=opp

%% 

<YYINITIAL> {ENTIER_SIMPLE}
      {
            return new Valeur(yytext());
      }

<YYINITIAL> {PLUS}
      {
            return new Plus(yytext());
      }

<YYINITIAL> {MINUS}
      {
            return new Minus(yytext());
      }


<YYINITIAL> {MULT}
      {
            return new Mult(yytext());
      }


<YYINITIAL> {QUO}
      {
            return new Quo(yytext());
      }


<YYINITIAL> {OPP}
      {
            return new Opp(yytext());
      }

<YYINITIAL,COMMENTAIRE> "/*"
      {
            yybegin(COMMENTAIRE) ;
            compteurCommentaire++;
      } 

<COMMENTAIRE> [^*]+|[*][^/]
      {
      }

<COMMENTAIRE> "*/"
      {
            compteurCommentaire--;
            if(compteurCommentaire==0)
                  { yybegin(YYINITIAL);}
      }


/* ajouter le cas des espaces et fins de ligne */

/* ajouter les autres tokens */
