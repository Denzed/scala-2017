package ru.spbau.jvm.scala.lecture04

object Task {

  val text: String =
    s"""
       |Требуется реализовать мультимножество.
       |
       |Требования:
       |1) методы filter, map, flatMap;
       |2) методы apply, unapplySeq (*) в companion object;
       |3) метод apply;
       |4) поиск элемента в множестве (возвращаемый тип: Option[A]);
       |5) пересечение (&) и объединение (|) множеств.
       |
       |Тесты должны покрывать добавление,
       |  поиск элементов,
       |  использование функций высшего порядка,
       |  for-comprehension синтаксис,
       |  паттерн-матчинг.
     """.stripMargin

}
