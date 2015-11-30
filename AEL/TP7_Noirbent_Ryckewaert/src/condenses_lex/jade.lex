package condenses_lex ;

%%

%class TokenizerV1
%implements Tokenizer
%public
%unicode
%line
%column

%{
%}

%%


<YYINITIAL>{
	[a-zA-Z]|(\\.)
	{
		return new Lettre(yytext());	
	}
	
	[0-9]+
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

}

[^/\s\n]
	{ 
		yybegin(YYINITIAL);
		return new Unknown(yytext());
	}
