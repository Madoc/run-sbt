package com.github.madoc.runsbt.running

import com.github.madoc.runsbt.running.SBTCommand.CombinedCommand

sealed trait SBTCommand {
  def :+(that:SBTCommand):SBTCommand = (this,that) match {
    case (CombinedCommand(a), CombinedCommand(b)) ⇒ CombinedCommand(a ++ b)
    case (_, CombinedCommand(a)) ⇒ CombinedCommand(this +: a)
    case (CombinedCommand(a), _) ⇒ CombinedCommand(a :+ that)
    case _ ⇒ CombinedCommand(Seq(this, that))
  }

  def toStringSeq:Seq[String]
}
object SBTCommand {
  sealed case class CombinedCommand(commands:Seq[SBTCommand]) extends SBTCommand {
    def toStringSeq = commands flatMap {_ toStringSeq}
  }
  sealed case class FreeFormCommand(text:String) extends SBTCommand {def toStringSeq = Seq(text)}

  object CleanCommand extends SBTCommand {override def toStringSeq = Seq("clean")}
  object CompileCommand extends SBTCommand {def toStringSeq = Seq("compile")}
  object PublishLocalCommand extends SBTCommand {def toStringSeq = Seq("publish-local")}
  object RunCommand extends SBTCommand {def toStringSeq = Seq("run")}
  object TestCommand extends SBTCommand {def toStringSeq = Seq("test")}
}
