package com.github.madoc.runsbt.running

sealed trait SBTCommand {
  def toStringSeq:Seq[String]
}
object SBTCommand {
  sealed case class FreeFormCommand(text:String) extends SBTCommand {def toStringSeq = Seq(text)}

  object CompileCommand extends SBTCommand {def toStringSeq = Seq("compile")}
  object PublishLocalCommand extends SBTCommand {def toStringSeq = Seq("publish-local")}
  object TestCommand extends SBTCommand {def toStringSeq = Seq("test")}
}
