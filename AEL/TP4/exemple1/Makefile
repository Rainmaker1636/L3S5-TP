#TP4 AEL octobre 2015
#Valentin Ryckewaert et Marion Noirbent

SRCPATH=src/exemple1/


gen:$(SRCPATH)exemple1.lex
	java -jar ../jflex-1.6.1.jar src/exemple1/exemple1.lex

comp:$(SRCPATH)TestEx1.java
	javac -cp src -d bin src/exemple1/TestEx1.java

test:comp
	@echo "\nExecution du test";
	@java -cp bin exemple1.TestEx1 test.txt;
	@echo "\n"

%.ylex:%.lex
	java -jar jflex-1.6.1.jar $*.lex

.PHONY:gen comp test
