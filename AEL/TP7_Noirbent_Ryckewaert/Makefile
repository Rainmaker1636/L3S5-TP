#TP4 AEL octobre 2015
#Valentin Ryckewaert et Marion Noirbent

SRCPATH=src/condenses_lex/
all : gen comp jadeRunner test2
test: gen comp testToken
gen:$(SRCPATH)jade.lex
	java -jar jflex-1.6.1.jar $(SRCPATH)jade.lex

comp:$(SRCPATH)Test.java
	javac -cp src:bin -d bin $(SRCPATH)Test.java

testToken:comp
	@echo "\nExecution du test\n";
	@java -cp bin condenses_lex.Test testToken.txt;

test2:comp
	@echo "\nExecution du test\n";
	@java -cp bin jade.JadeRunner testToken.txt;

jadeRunner:
	javac -cp src:bin -d bin src/jade/JadeRunner.java

clean:
	rm -f *~
	rm -f *#
	rm -f src/condenses_lex/*~
	rm -f src/condenses_lex/*#

%.ylex:%.lex
	java -jar jflex-1.6.1.jar $*.lex

.PHONY:gen comp test
