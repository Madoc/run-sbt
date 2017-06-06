package com.github.madoc.runsbt.async

import scala.concurrent.Future

/** Similar to a `Stream` in that consecutive elements are provided in a delayed fashion, but unlike `Stream`, this
  * type provides following elements as a `Future` of the remaining elements. Only the minimum set of methods
  * required for this type's immediate use are implemented here, and nothing more. Notably, this type does not
  * implement most of the features for Scala `Seq` subtypes. */
sealed trait FList[+A]
object FList {
  object Nil extends FList[Nothing]
  sealed case class Cons[A](head:A, tail:Future[FList[A]]) extends FList[A]
}
