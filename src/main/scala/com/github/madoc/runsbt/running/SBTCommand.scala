package com.github.madoc.runsbt.running

sealed trait SBTCommand {
  def toStringSeq:Seq[String]
}
object SBTCommand {
  sealed case class FreeForm(text:String) extends SBTCommand {def toStringSeq = Seq(text)}
  object PublishLocalCommand extends SBTCommand {def toStringSeq = Seq("publish-local")}
  object TestCommand extends SBTCommand {def toStringSeq = Seq("test")}
}
