package ru.spbau.jvm.scala.lecture04;

public class Main {
    public static void main(String[] args) {
        ComplexNumber complexNumber = new ComplexNumber(0, 0);
        complexNumber.$plus(ComplexNumber$.MODULE$.apply(1, 1));
    }
}
