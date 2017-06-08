package com.github.madoc.runsbt.events

sealed trait SBTEvent
object SBTEvent {
  object DoneUpdating extends SBTEvent
  sealed case class LoadingPlugins(pluginPath:String) extends SBTEvent
  sealed case class LoadingProjectDefinition(path:String) extends SBTEvent
  sealed case class ModuleNotFound(module:String, tried:Seq[ModuleNotFound_Tried]) extends SBTEvent
  sealed case class Resolving(dependency:String) extends SBTEvent
  sealed case class SetProject(projectName:String, buildPath:String) extends SBTEvent
  sealed case class UnrecognizedEvent(line:String) extends SBTEvent
  sealed case class UnresolvedDependencies(dependencies:Seq[String], stackTrace:Seq[String]) extends SBTEvent
  sealed case class Updating(pathDescription:String) extends SBTEvent

  case class ModuleNotFound_Tried(scope:String, uris:Seq[String])
}
