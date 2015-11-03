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
%state JUMPX
%state JUMPY
%state TIMES

%{
	int compteurCommentaire = 0;
	int lastInt;
	StepLength pas=new StepLength(1);		
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


\/\/[^\n]*
{
	compteurCommentaire++;
	yybegin(COMMENTAIRE);
}

\/\*([^*]+|[*]+[^/])*
{
	compteurCommentaire++;
	yybegin(COMMENTAIRE_MULTI);
}


(pas|PAS)
{
	yybegin(STEP);
}

(aller|ALLER)
{
	yybegin(JUMPX);
}

{SEPARATEUR}
	{}

<TIMES>{
 	\s*fois
	{	
		return new Repeat(lastInt);
	}
}


<COMMENTAIRE>{
	"\n"
	{
		compteurCommentaire--;
		if (compteurCommentaire == 0 ){
			yybegin(YYINITIAL);
		}
	}
}

<COMMENTAIRE_MULTI>{
	"*/"
	{
		compteurCommentaire--;
		if (compteurCommentaire == 0){
			yybegin(YYINITIAL);
		}
	}
}

<STEP>{
	[0-9]+
	{
		pas=new StepLength(Integer.parseInt(yytext()));
		return pas;

	}	
}


<JUMPX>{
	-?[0-9]+
	{
		lastInt=Integer.parseInt(yytext());
		yybegin(JUMPY);
	}
}


<JUMPY>{
	-?[0-9]+
	{
		return new Jump(lastInt,Integer.parseInt(yytext()));
	}
}


[0-9]+
{
	lastInt=Integer.parseInt(yytext());
	yybegin(TIMES);
}
[^/\s\n]+
	{ return new Unknown(yytext());}

