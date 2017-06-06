package com.github.madoc.runsbt.async

import scala.concurrent.{Future, Promise, SyncVar}

/** Similar to a `Stream` in that consecutive elements are provided in a delayed fashion, but unlike `Stream`, this
  * type provides following elements as a `Future` of the remaining elements. Only the minimum set of methods
  * required for this type's immediate use are implemented here, and nothing more. Notably, this type does not
  * implement most of the features for Scala `Seq` subtypes. */
sealed trait FList[+A]
object FList {
  object Nil extends FList[Nothing]
  sealed case class Cons[A](head:A, tail:Future[FList[A]]) extends FList[A]

  trait Receiver[A] {
    def receive(a:A)
    def close()
  }

  def newPair[A]():(Future[FList[A]],Receiver[A]) = {
    sealed trait State
    sealed case class Open(promise:Promise[FList[A]]) extends State
    object Closed extends State

    val state:SyncVar[State] = new SyncVar[State]()

    val receiver:Receiver[A] = new Receiver[A] {
      def receive(a:A) {state take match {
        case Closed ⇒ state put Closed; sys error "continued to receive after receiver was closed"
        case Open(promise) ⇒
          val nextPromise = Promise[FList[A]]()
          promise success Cons(a, nextPromise future)
          state put Open(nextPromise)
      }}
      def close() {state take match {
        case Closed ⇒ state put Closed
        case Open(promise) ⇒ promise success Nil; state put Closed
      }}
    }

    val firstPromise = Promise[FList[A]]()
    state put Open(firstPromise)
    (firstPromise future, receiver)
  }
}
