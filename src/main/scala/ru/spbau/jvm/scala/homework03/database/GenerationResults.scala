package ru.spbau.jvm.scala.homework03.database

trait GenerationResult

case object GenerationSuccessful extends GenerationResult

case object GenerationFailed extends GenerationResult
