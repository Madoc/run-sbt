package com.github.madoc.runsbt.events

import com.github.madoc.runsbt.events.SBTEvent._

object SBTEventParser extends (Stream[String]⇒Stream[SBTEvent]) {
  def apply(in:Stream[String]):Stream[SBTEvent] = defaultState(in filterNot ignoredLine)

  private val justColons = """^\[\w+\]\s*:+\s*$""".r.pattern
  private val justLogLevel = """^\[\w+\]\s*$""".r.pattern
  private def ignoredLine(line:String):Boolean =
    line.trim.isEmpty || justColons.matcher(line).matches() || justLogLevel.matcher(line).matches()

  private object Patterns {
    val PdoneUpdating = """\[info\] Done updating.""".r
    val PloadingPluginsFrom = """\[info\] Loading global plugins from (.*)""".r
    val PloadingProjectDefinition = """\[info\] Loading project definition from (.*)""".r
    val Presolving = """\[info\] Resolving (.*) ...""".r
    val Presolving_moduleNotFound = """\[warn\] 	module not found: (.*)""".r
    val Presolving_moduleNotFound_tried = """\[warn\] ==== (.*): tried""".r
    val Presolving_moduleNotFound_tried_uri = """\[warn\]   (.*)""".r
    val PsetProject = """\[info\] Set current project to (.*) \(in build (.*)\)""".r
    val PunresolvedDependencies = """\[warn\]\s+::\s+UNRESOLVED DEPENDENCIES\s+::""".r
    val Pupdating = """\[info\] Updating (.*)...""".r
  }
  import Patterns._

  private def defaultState(in:Stream[String]):Stream[SBTEvent] = in match {
    case PdoneUpdating() #:: in2 ⇒ DoneUpdating #:: defaultState(in2)
    case PloadingPluginsFrom(pluginPath) #:: in2 ⇒ LoadingPlugins(pluginPath) #:: defaultState(in2)
    case PloadingProjectDefinition(path) #:: in2 ⇒ LoadingProjectDefinition(path) #:: defaultState(in2)
    case Presolving(dependency) #:: in2 ⇒ resolvingState(dependency, in2)
    case PsetProject(projectName, buildPath) #:: in2 ⇒ SetProject(projectName, buildPath) #:: defaultState(in2)
    case PunresolvedDependencies() #:: in2 ⇒ unresolvedDependenciesState(in2)
    case Pupdating(pathDesc) #:: in2 ⇒ Updating(pathDesc) #:: defaultState(in2)
    case line #:: in2 ⇒ UnrecognizedEvent(line) #:: defaultState(in2)
    case Stream.Empty ⇒ Stream.Empty
  }

  private def resolvingState(dependency:String, in:Stream[String]):Stream[SBTEvent] = {
    def moduleNotFoundSubState(module:String, in:Stream[String], tried:Seq[ModuleNotFound_Tried]=Seq()):(ModuleNotFound, Stream[String]) = {
      def triedSubState(searchScope:String, in:Stream[String], uris:Seq[String]=Seq()):(ModuleNotFound_Tried,Stream[String]) = in match {
        case Presolving_moduleNotFound_tried_uri(uri) #:: in2 ⇒ triedSubState(searchScope, in2, uris :+ uri)
        case _ ⇒ (ModuleNotFound_Tried(searchScope, uris), in)
      }

      in match {
        case Presolving_moduleNotFound_tried(searchScope:String) #:: in2 ⇒
          val (nextTried, in3) = triedSubState(searchScope, in2)
          moduleNotFoundSubState(module, in3, tried :+ nextTried)
        case _ ⇒ (ModuleNotFound(module, tried), in)
      }
    }

    in match {
      case Presolving_moduleNotFound(module) #:: in2 ⇒
        val (notFound, in3) = moduleNotFoundSubState(module, in2)
        notFound #:: defaultState(in3)
      case _ ⇒ Resolving(dependency) #:: defaultState(in)
    }
  }

  private def unresolvedDependenciesState(in:Stream[String]):Stream[SBTEvent] = {
    // TODO
    ???
  }
}
