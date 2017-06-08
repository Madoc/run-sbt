package com.github.madoc.runsbt.events

sealed trait SBTEvent
object SBTEvent {
  object DoneUpdating extends SBTEvent
  sealed case class LoadingProjectDefinition(path:String) extends SBTEvent
  sealed case class Updating(pathDescription:String) extends SBTEvent
}