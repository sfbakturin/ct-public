all: Main.class Conjunctio.class Disjunctio.class Implicatio.class Inversio.class Variable.class ExpressionParser.class CharIterator.class

run:
	java Main

clean:
	rm *.class

zip:
	zip lab.zip *.java Makefile

Main.class: Main.java
	javac -cp . Main.java

Conjunctio.class: Conjunctio.java
	javac -cp . Conjunctio.java

Implicatio.class: Implicatio.java
	javac -cp . Implicatio.java

Disjunctio.class: Disjunctio.java
	javac -cp . Disjunctio.java

Inversio.class: Inversio.java
	javac -cp . Inversio.java

Variable.class: Variable.java
	javac -cp . Variable.java

ExpressionParser.class: ExpressionParser.java
	javac -cp . ExpressionParser.java

CharIterator.class: CharIterator.java
	javac -cp . CharIterator.java
