package com.github.madoc.runsbt.running

import com.github.madoc.runsbt.async.FList

import scala.concurrent.{ExecutionContext, Future}
import scala.sys.process._

trait SBTProcess {
  def cancel()
  def exitValue:Future[Int]
  def logLines:Future[FList[String]]
}
object SBTProcess {
  case class BasedOnProcessBuilder(builder:ProcessBuilder)(implicit ec:ExecutionContext) extends SBTProcess {
    private val (_logLines, logLinesReceiver) = FList.newPair[String]()
    private val process:Process = builder.run(ProcessLogger(logLinesReceiver.receive _))

    val exitValue:Future[Int] = Future {
      val ev = process exitValue()
      logLinesReceiver close()
      ev
    }

    def cancel() {process destroy(); logLinesReceiver close()}
    def logLines:Future[FList[String]] = _logLines
  }
}
