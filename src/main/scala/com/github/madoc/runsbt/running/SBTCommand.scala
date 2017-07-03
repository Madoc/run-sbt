package com.github.madoc.runsbt.running

import com.github.madoc.runsbt.running.SBTCommand.CommandEffect._
import com.github.madoc.runsbt.running.SBTCommand.{CombinedCommand, CommandEffect}

sealed trait SBTCommand {
  def :+(that:SBTCommand):SBTCommand = (this,that) match {
    case (CombinedCommand(a), CombinedCommand(b)) ⇒ CombinedCommand(a ++ b)
    case (_, CombinedCommand(a)) ⇒ CombinedCommand(this +: a)
    case (CombinedCommand(a), _) ⇒ CombinedCommand(a :+ that)
    case _ ⇒ CombinedCommand(Seq(this, that))
  }

  def implies(effect:CommandEffect):Boolean
  def toStringSeq:Seq[String]
}
object SBTCommand {
  sealed case class CombinedCommand(commands:Seq[SBTCommand]) extends SBTCommand {
    def implies(effect:CommandEffect) = commands exists {_ implies effect}
    def toStringSeq = commands flatMap {_ toStringSeq}
  }
  sealed case class FreeFormCommand(text:String) extends SBTCommand {
    def implies(effect:CommandEffect) = false
    def toStringSeq = Seq(text)
  }

  object AssemblyCommand extends SBTCommand {
    def implies(effect:CommandEffect) = effect match {
      case AssemblyEffect | CompilationEffect ⇒ true
      case _ ⇒ false
    }
    override def toStringSeq = Seq("assembly")
  }
  object CleanCommand extends SBTCommand {
    def implies(effect:CommandEffect) = false
    def toStringSeq = Seq("clean")
  }
  object CompileCommand extends SBTCommand {
    def implies(effect:CommandEffect) = effect match {case CompilationEffect ⇒ true; case _ ⇒ false}
    def toStringSeq = Seq("compile")
  }
  object PublishLocalCommand extends SBTCommand {
    def implies(effect:CommandEffect) = effect match {
      case CompilationEffect | PublishLocalEffect | TestEffect ⇒ true
      case _ ⇒ false
    }
    def toStringSeq = Seq("publish-local")}
  object RunCommand extends SBTCommand {
    def implies(effect:CommandEffect) = effect match {
      case CompilationEffect | RunEffect ⇒ true
      case _ ⇒ false
    }
    def toStringSeq = Seq("run")
  }
  object TestCommand extends SBTCommand {
    def implies(effect:CommandEffect) = effect match {
      case CompilationEffect | TestEffect ⇒ true
      case _ ⇒ false
    }
    def toStringSeq = Seq("test")}
  object UpdateCommand extends SBTCommand {
    def implies(effect:CommandEffect) = false
    def toStringSeq = Seq("update")}

  val statics:Set[SBTCommand] = Set(AssemblyCommand, CleanCommand, CompileCommand, PublishLocalCommand, RunCommand,
    TestCommand, UpdateCommand)
  val staticsByName:Map[String,SBTCommand] = statics map {s ⇒ ((s toStringSeq) mkString " ", s)} toMap
  def apply(text:String):SBTCommand = staticsByName getOrElse (text, FreeFormCommand(text))

  sealed trait CommandEffect
  object CommandEffect {
    object AssemblyEffect extends CommandEffect
    object CompilationEffect extends CommandEffect
    object PublishLocalEffect extends CommandEffect
    object RunEffect extends CommandEffect
    object TestEffect extends CommandEffect
  }
}
