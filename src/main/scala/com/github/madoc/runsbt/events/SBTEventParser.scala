package com.github.madoc.runsbt.events

import com.github.madoc.runsbt.events.SBTEvent.{DoneUpdating, LoadingProjectDefinition, Updating}

object SBTEventParser extends (Stream[String]⇒Stream[SBTEvent]) {
  def apply(in:Stream[String]):Stream[SBTEvent] = defaultState(in filterNot {_.trim isEmpty})

  private object Patterns {
    val PdoneUpdating = """\[info\] Done updating.""".r
    val PloadingProjectDefinition = """\[info\] Loading project definition from (.*)""".r
    val Presolving = """\[info\] Resolving (.*) ...""".r
    val Pupdating = """\[info\] Updating (.*)...""".r
  }
  import Patterns._

  private def defaultState(in:Stream[String]):Stream[SBTEvent] = in match {
    case PdoneUpdating() #:: in2 ⇒ DoneUpdating #:: defaultState(in2)
    case PloadingProjectDefinition(path) #:: in2 ⇒ LoadingProjectDefinition(path) #:: defaultState(in2)
    case Presolving(dependency) #:: in2 ⇒ resolvingState(dependency, in2)
    case Pupdating(pathDesc) #:: in2 ⇒ Updating(pathDesc) #:: defaultState(in2)
  }

  private def resolvingState(dependency:String, in:Stream[String]):Stream[SBTEvent] = {
    in match {
      case Presolving(dependency) #:: in2 ⇒
      case _ ⇒ defaultState(in)
    }
  }
}
