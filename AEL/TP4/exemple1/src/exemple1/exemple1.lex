/* Exemple 1 */
package exemple1;

%%

%unicode

MOT_USUEL=[:letter:]+
ENTIER_SIMPLE=[0-9]+
IDENTIFICATEUR=[a-zA-Z](_?[a-zA-Z0-9])*
ENTIER=(0(([0-7]*_?[0-7]+)+)|(0x([a-fA-F0-9]*_?[a-fA-F0-9]+)+))
OPERATEUR=[\+\-\*/]|"opp"|"plus"|"minus"|"quo"

%% 

{MOT_USUEL}|{ENTIER_SIMPLE}|{IDENTIFICATEUR}|{ENTIER}|{OPERATEUR}
      {return new Yytoken(yytext());}

[^[:letter:]0-9]+
      {}  

