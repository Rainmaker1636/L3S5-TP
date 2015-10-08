/* Exemple 1 */
package exemple1;

%%

%unicode

MOT_USUEL=[:letter:]+
ENTIER_SIMPLE=[0-9]+

%% 

{MOT_USUEL}|{ENTIER_SIMPLE}
      {return new Yytoken(yytext());}


[^[:letter:]0-9]+
      {}  
