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
%state JUMPX
%state JUMPY
%state TIMES
%state STEP

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
norde|NORDE
	{ return new Move(Direction.NORTHE);}
sude|SUDE
	{ return new Move(Direction.SOUTHE);}
nordw|NORDW
	{ return new Move(Direction.NORTHW);}
sudw|SUDW
	{ return new Move(Direction.SOUTHW);}

pas|PAS
	{yybegin(STEP);}
aller|ALLER
	{yybegin(JUMPX);}
"//"
{
	compteurCommentaire++;
	yybegin(COMMENTAIRE);
}
"/*"
{
	compteurCommentaire++;
	yybegin(COMMENTAIRE_MULTI);
}


<YYINITIAL>
{
	[0-9]+
	{
		lastInt=Integer.parseInt(yytext());
		yybegin(TIMES);
	}
}

<COMMENTAIRE>{
	[^\n]+\n
	{
		compteurCommentaire--;
		if (compteurCommentaire == 0 ){
			yybegin(YYINITIAL);
		}
	}
}

<COMMENTAIRE_MULTI>{
	([^*]+|[*]+[^/])*\*\/
	{
		compteurCommentaire--;
		if (compteurCommentaire == 0){		
			yybegin(YYINITIAL);
		}
	}
}

<JUMPX>{
	\s+
	{}
	
	-?[0-9]+
	{
		lastInt=Integer.parseInt(yytext());
		yybegin(JUMPY);	
	}
}


<JUMPY>{
	\s+
	{}
	
	-?[0-9]+
	{
		yybegin(YYINITIAL);
		return new Jump(lastInt,Integer.parseInt(yytext()));
	}
}

<TIMES>{
 	\s*(fois|FOIS)
	{	
		yybegin(YYINITIAL);
		return new Repeat(lastInt);
	}
}


<STEP>{
	\s+
	{
	}

	[0-9]+
	{
		pas=new StepLength(Integer.parseInt(yytext()));
		yybegin(YYINITIAL);
		return new StepLength(Integer.parseInt(yytext()));
	}	
}
[^/\s\n]+
	{ 
		yybegin(YYINITIAL);
		return new Unknown(yytext());
	}
{SEPARATEUR}
	{}

