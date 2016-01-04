package condenses_lex ;

%%

%class TokenizerV1
%implements Tokenizer
%public
%unicode
%line
%column

%state ECHAPPEMENT

%{
%}
echap=[\\]
lettre=[a-zA-Z]
entier=[0-9]+
%%


<YYINITIAL>{
	{lettre}
	{
		return new Lettre(yytext());	
	}
	
	{entier}
	{
		return new Entier(Integer.parseInt(yytext()));
	}

	\(
	{
		return new Ouvrante(true);
	}

	\)
	{
		return new Fermante(false);
	}

	{echap}
	{
		yybegin(ECHAPPEMENT);
	}

	#
	{
 		return new Eod("Fin");
	}
	
}

<ECHAPPEMENT>{
	{entier}
	{
		yybegin(YYINITIAL);
		return new Lettre(yytext());
	}

	{lettre}
	{
		yybegin(YYINITIAL);
		return new Lettre(yytext());
	}
}

[^/\s\n]
{ 
	return new Unknown(yytext());
}

