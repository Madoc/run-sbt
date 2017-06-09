package com.github.madoc.runsbt.events

import com.github.madoc.runsbt.events.SBTEvent._

object SBTEventParser extends (Stream[String]⇒Stream[SBTEvent]) {
  def apply(in:Stream[String]):Stream[SBTEvent] = defaultState(in filterNot ignoredLine)

  private val justColons = """^\[\w+\]\s*:+\s*$""".r.pattern
  private val justLogLevel = """^\[\w+\]\s*$""".r.pattern
  private def ignoredLine(line:String):Boolean =
    line.trim.isEmpty || justColons.matcher(line).matches() || justLogLevel.matcher(line).matches()

  private object Patterns {
    val Pcompiling = """\[info\] Compiling (.+) (.+) source(?:s)? to (.+)...""".r
    val Pcompiling_featureWarnings = """\[warn\] there (?:were|was) (.*) feature warning(?:s)?; re-run with -feature for details""".r
    val Pcompiling_warningsFound = """\[warn\] (.*) warning(?:s)? found""".r
    val PcompletionLine = """\[(\w+)\] Total time: (.+), completed (.+)""".r
    val PdoneUpdating = """\[info\] Done updating.""".r
    val Pexception_firstLine = """(.+: .+)""".r
    val Pexception_nextLines = """\s+(at .+)""".r
    val PloadingPluginsFrom = """\[info\] Loading global plugins from (.*)""".r
    val PloadingProjectDefinition = """\[info\] Loading project definition from (.*)""".r
    val Presolving = """\[info\] Resolving (.*) ...""".r
    val Presolving_moduleNotFound = """\[warn\] 	module not found: (.*)""".r
    val Presolving_moduleNotFound_tried = """\[warn\] ==== (.*): tried""".r
    val Presolving_moduleNotFound_tried_uri = """\[warn\]   (.*)""".r
    val PsetProject = """\[info\] Set current project to (.*) \(in build (.*)\)""".r
    val PstageError = """\[error\] \(([^)]+)\) (.+)""".r
    val PunresolvedDependencies = """\[warn\]\s+::\s+UNRESOLVED DEPENDENCIES\s+::""".r
    val PunresolvedDependencies_ignore1 = """\[warn\]\s+Note: Unresolved dependencies path:""".r
    val PunresolvedDependencies_ignore2 = """\[warn\]\s+.+ \(.+\)""".r
    val PunresolvedDependencies_ignore3 = """\[warn\]\s+\+- .*""".r
    val PunresolvedDependencies_notFound = """\[warn\]\s+:: (.*): not found""".r
    val Pupdating = """\[info\] Updating (.*)...""".r
  }
  import Patterns._

  private def defaultState(in:Stream[String]):Stream[SBTEvent] = in match {
    case Pcompiling(number, language, targetDirectory) #:: in2 ⇒ compilingState(in2, number, language, targetDirectory)
    case PcompletionLine(status, duration, completionTime) #:: in2 ⇒ CompletionLine(status, duration, completionTime) #:: defaultState(in2)
    case PdoneUpdating() #:: in2 ⇒ DoneUpdating #:: defaultState(in2)
    case PloadingPluginsFrom(pluginPath) #:: in2 ⇒ LoadingPlugins(pluginPath) #:: defaultState(in2)
    case PloadingProjectDefinition(path) #:: in2 ⇒ LoadingProjectDefinition(path) #:: defaultState(in2)
    case Presolving(dependency) #:: in2 ⇒ resolvingState(dependency, in2)
    case PsetProject(projectName, buildPath) #:: in2 ⇒ SetProject(projectName, buildPath) #:: defaultState(in2)
    case PstageError(stage, errorMessage) #:: in2 ⇒ StageError(stage, errorMessage) #:: defaultState(in2)
    case PunresolvedDependencies() #:: in2 ⇒ unresolvedDependenciesState(in2)
    case Pupdating(pathDesc) #:: in2 ⇒ Updating(pathDesc) #:: defaultState(in2)
    case line #:: in2 ⇒ UnrecognizedEvent(line) #:: defaultState(in2)
    case Stream.Empty ⇒ Stream.Empty
  }

  private def compilingState(
    in:Stream[String], numberOfSourceFiles:String, sourceLanguage:String, targetDirectory:String,
    _featureWarnings:Option[String]=None, _numberOfWarnings:Option[String]=None
  ):Stream[SBTEvent] = {
    def recurse(
      in2:Stream[String],
      featureWarnings:Option[String]=_featureWarnings, numberOfWarnings:Option[String]=_numberOfWarnings):Stream[SBTEvent] =
      compilingState(in2, numberOfSourceFiles, sourceLanguage, targetDirectory, featureWarnings, numberOfWarnings)

    in match {
      case Pcompiling_featureWarnings(number) #:: in2 ⇒ recurse(in2, featureWarnings=Some(number))
      case Pcompiling_warningsFound(number) #:: in2 ⇒ recurse(in2, numberOfWarnings=Some(number))
      case _ ⇒ Compiling(
        sourceFileCount = parseNumber(numberOfSourceFiles),
        sourceLanguage = sourceLanguage,
        targetDirectory = targetDirectory,
        warningsCount = _numberOfWarnings map parseNumber getOrElse 0,
        featureWarningsCount = _featureWarnings map parseNumber getOrElse 0
      ) #:: defaultState(in)
    }
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

  private def unresolvedDependenciesState(in:Stream[String], dependencies:Seq[String]=Seq()):Stream[SBTEvent] = {
    def notFoundSubState(dependency:String, in:Stream[String]):Stream[SBTEvent] = in match {
      case PunresolvedDependencies_ignore1() #:: in2 ⇒ notFoundSubState(dependency, in2)
      case PunresolvedDependencies_ignore2() #:: in2 ⇒ notFoundSubState(dependency, in2)
      case PunresolvedDependencies_ignore3() #:: in2 ⇒ notFoundSubState(dependency, in2)
      case _ ⇒ unresolvedDependenciesState(in, dependencies :+ dependency)
    }

    in match {
      case PunresolvedDependencies_notFound(dependency) #:: in2 ⇒ notFoundSubState(dependency, in2)
      case _ ⇒
        val (stackTrace, in2) = extractStackTrace(in)
        UnresolvedDependencies(dependencies, stackTrace) #:: defaultState(in2)
    }
  }

  private def extractStackTrace(in:Stream[String]):(Seq[String],Stream[String]) = {
    def extractAtLinesSubState(in:Stream[String], stackTrace:Seq[String]):(Seq[String],Stream[String]) = in match {
      case Pexception_nextLines(line) #:: in2 ⇒ extractAtLinesSubState(in2, stackTrace :+ line)
      case _ ⇒ (stackTrace, in)
    }

    in match {
      case Pexception_firstLine(fst) #:: in2 ⇒ extractAtLinesSubState(in2, Seq(fst))
      case _ ⇒ (Seq(), in)
    }
  }

  private def parseNumber(number:String):Int = number match {
    case "one" ⇒ 1
    case _ ⇒ number.trim toInt
  }
}
