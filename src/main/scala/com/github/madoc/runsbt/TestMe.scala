package com.github.madoc.runsbt

import com.github.madoc.runsbt.async.FList
import com.github.madoc.runsbt.config.ExecutableConfig.CommandLineExecutableConfig
import com.github.madoc.runsbt.config.SBTConfig
import com.github.madoc.runsbt.running.SBTCommand

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Success}

object TestMe extends App {
  import scala.concurrent.ExecutionContext.Implicits.global
  val proc = RunSBT(SBTConfig(CommandLineExecutableConfig("/Users/madoc/bin/sbt")))(SBTCommand.FreeForm("-h"))
  proc.exitValue.onComplete({ev ⇒ println(s"***** EXIT VALUE $ev")})
  printFList(proc.logLines)

  val finished = Promise[Unit]()

  def printFList(f:Future[FList[String]]) {f.onComplete({
    case Success(list) ⇒ list match {
      case FList.Nil ⇒ println("***** END OF LINES"); finished.success(Unit)
      case FList.Cons(head, tail) ⇒ println(s"### $head"); printFList(tail)
    }
    case Failure(exception) ⇒ finished.success(Unit); throw exception
  })}

  Await.ready(finished future, Duration.Inf)
}
