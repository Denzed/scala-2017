package ru.spbau.jvm.scala.homework01.util

object ReflectionHelper {
  def getPrimaryConstructor[T](clazz: java.lang.Class[T])(args:AnyRef*): T =
    clazz.getConstructors()(0).newInstance(args:_*).asInstanceOf[T]
}
