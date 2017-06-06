package com.github.madoc.runsbt.events

sealed trait SBTEvents
object SBTEvents {
  object NilSBTEvents extends SBTEvents
  sealed case class SBTEventList(head:SBTEvent, tail:SBTEvents) extends SBTEvents
}
