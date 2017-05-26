package com.github.madoc.runsbt.running

sealed trait SBTCommand
object SBTCommand {
  object PublishLocalCommand extends SBTCommand
  object TestCommand extends SBTCommand
}
