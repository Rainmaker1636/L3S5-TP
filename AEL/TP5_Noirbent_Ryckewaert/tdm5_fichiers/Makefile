#TP4 AEL octobre 2015
#Valentin Ryckewaert et Marion Noirbent

SRCPATH=src/jadelex/


gen:$(SRCPATH)jade.lex
	java -jar ../jflex-1.6.1.jar $(SRCPATH)jade.lex

comp:$(SRCPATH)Test.java
	javac -cp src -d bin $(SRCPATH)Test.java

test:comp
	@echo "\nExecution du test";
	@java -cp bin jadelex.Test testToken.txt;

%.ylex:%.lex
	java -jar jflex-1.6.1.jar $*.lex

.PHONY:gen comp test
