package com.github.madoc.runsbt.running

import java.io.File
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

import com.github.madoc.runsbt.events.{SBTEvent, SBTEventParser}

import scala.sys.process._

trait SBTProcess {
  def cancel()
  lazy val events:Stream[SBTEvent] = SBTEventParser(outputLines)
  def exitValue:Int
  def outputLines:Stream[String]
}
object SBTProcess {
  case class BasedOnProcessBuilder(builder:ProcessBuilder, workingDirectory:File, command:SBTCommand) extends SBTProcess {
    def exitValue = process exitValue
    def cancel() {process destroy(); buffer put EOS}
    lazy val outputLines:Stream[String] = nextLogLine()

    private sealed trait Element
    private sealed case class Line(string:String) extends Element
    private object EOS extends Element

    private val buffer:BlockingQueue[Element] = new LinkedBlockingQueue[Element]()
    private val process:Process = builder.run(ProcessLogger(str ⇒ buffer put Line(str)))

    private def nextLogLine():Stream[String] = buffer take match {
      case EOS ⇒ buffer put EOS; Stream.empty
      case Line(string) ⇒ Stream cons (cleanUp(string), nextLogLine())
    }

    private def cleanUp(str:String):String = str replaceAll ("\\e\\[[\\d;]*[^\\d;]", "")

    locally {new Thread() {
      override def run() {process.exitValue(); buffer put EOS}
    }.start()}

    override def toString:String = s"SBTProcess(directory=$workingDirectory, command=$command, process=$builder)"
  }
}
