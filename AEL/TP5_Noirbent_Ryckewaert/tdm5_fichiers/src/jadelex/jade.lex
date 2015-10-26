package jadelex ;

import jade.Direction;
%%

%class TokenizerV1
%implements Tokenizer
%public
%unicode
%line
%column

%state COMMENTAIRE
%state COMMENTAIRE_MULTI
%state STEP

%{
	int compteurCommentaire = 0;
%}

SEPARATEUR=[\s\n]

%%

lever|LEVER
	{ return new PenMode(false);}

baisser|BAISSER
	{ return new PenMode(true);}
nord|NORD
	{ return new Move(Direction.NORTH);}
sud|SUD
	{ return new Move(Direction.SOUTH);}
est|EST
	{ return new Move(Direction.EAST);}
ouest|OUEST
	{ return new Move(Direction.WEST);}


{SEPARATEUR}
	{}

"//"
{
	yybegin(COMMENTAIRE);
	compteurCommentaire++;
}

"/*"
{
	yybegin(COMMENTAIRE_MULTI);
	compteurCommentaire++;
}

<COMMENTAIRE>{
	[^\n]
	{}
	"\n"
	{
		compteurCommentaire--;
		if (compteurCommentaire == 0 ){
			yybegin(YYINITIAL);
		}
	}
}

<COMMENTAIRE_MULTI>{
	[^*]+[*][^/]
	{}
	"*/"
	{
		compteurCommentaire--;
		if (compteurCommentaire == 0){
			yybegin(YYINITIAL);
		}
	}
	
}
Si état PAS alors <yybegin(STEP)>
<STEP>{
	(pas|PAS)\s*
	{
		[0-9]+
		{
			yybegin(YYINITIAL);
			return new StepLength(Integer.parseInt(yytext()););
		}
	}
		
	
}



[^/\s\n]+
	{ return new Unknown("bonjour");}

