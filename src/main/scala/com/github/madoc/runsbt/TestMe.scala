package com.github.madoc.runsbt

import java.util.concurrent.Executors

import com.github.madoc.runsbt.async.FList
import com.github.madoc.runsbt.config.ExecutableConfig.CommandLineExecutableConfig
import com.github.madoc.runsbt.config.SBTConfig
import com.github.madoc.runsbt.running.SBTCommand

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.Duration

object TestMe extends App {
  // TODO tasks get submitted even after everything finished
  val threadPool = Executors.newFixedThreadPool(10)
  implicit val executionContext = ExecutionContext.fromExecutorService(threadPool)

  val proc = RunSBT(SBTConfig(CommandLineExecutableConfig("/Users/madoc/bin/sbt")))(SBTCommand.FreeForm("-h"))
  proc.exitValue.onComplete({ev ⇒ println(s"***** EXIT VALUE $ev")})

  FList.foreach(proc logLines)(println)

  Await.ready(FList completion (proc logLines), Duration.Inf)
  Await.ready(proc exitValue, Duration.Inf)
  //synchronized(wait(3000))
  threadPool.shutdown()
}
