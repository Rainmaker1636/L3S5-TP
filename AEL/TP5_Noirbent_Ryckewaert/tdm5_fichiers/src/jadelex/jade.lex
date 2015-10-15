package jadelex ;

%%

%class TokenizerV1
%implements Tokenizer
%public
%unicode
%line
%column

SEPARATEUR=[\s\n]
COMMENTAIRE_SIMPLE=(\/\/[^\n]*\n)
COMMENTAIRE_MULTI=(\/\*([^\*]|\**[^\/])*\*\/)
COMMENTAIRE={COMMENTAIRE_SIMPLE}|{COMMENTAIRE_MULTI}

%%

lever|LEVER
	{ return new PenMode(false);}

baisser|BAISSER
	{ return new PenMode(true);}





{SEPARATEUR}|{COMMENTAIRE}
	{}

[^/\s\n]+
	{ return new Unknown("bonjour");}

	
