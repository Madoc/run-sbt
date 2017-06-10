package com.github.madoc.runsbt.events

import com.github.madoc.runsbt.events.SBTEvent.{Running, UnrecognizedEvent}

private object SBTRunExtractor extends (Stream[SBTEvent]⇒Stream[SBTEvent]) {
  private val Prunning = """\[info\] Running (.*) """.r
  def apply(in:Stream[SBTEvent]):Stream[SBTEvent] = in match {
    case UnrecognizedEvent(Prunning(mainClass)) #:: in2 ⇒ runningState(in2, Seq(), mainClass)
    case head #:: in2 ⇒ head #:: apply(in2)
    case Stream.Empty ⇒ Stream.Empty
  }

  private def runningState(in:Stream[SBTEvent], lines:Seq[String], mainClass:String):Stream[SBTEvent] = in match {
    case UnrecognizedEvent(line) #:: in2 ⇒ runningState(in2, lines :+ line, mainClass)
    case _ ⇒ Running(mainClass, lines) #:: apply(in)
  }
}
