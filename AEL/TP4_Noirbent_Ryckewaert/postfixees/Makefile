#TP4 AEL octobre 2015
#Valentin Ryckewaert et Marion Noirbent

SRCPATH=src/postfixees/


gen:$(SRCPATH)postfixees.lex
	java -jar ../jflex-1.6.1.jar src/postfixees/postfixees.lex

comp:$(SRCPATH)Evaluateur.java
	javac -cp src -d bin src/postfixees/Evaluateur.java

test:comp
	@echo "\nExecution du test";
	@java -cp bin postfixees.Evaluateur testPost.txt;
	@echo "\n"

%.ylex:%.lex
	java -jar jflex-1.6.1.jar $*.lex

.PHONY:gen comp test
