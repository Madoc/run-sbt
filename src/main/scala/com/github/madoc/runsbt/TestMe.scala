package com.github.madoc.runsbt

import java.io.File

import com.github.madoc.runsbt.config.ExecutableConfig.CommandLineExecutableConfig
import com.github.madoc.runsbt.config.SBTConfig
import com.github.madoc.runsbt.events.{SBTEvent, SBTEventParser}
import com.github.madoc.runsbt.running.SBTCommand.CompileCommand

object TestMe extends App {
  def runOnce() {
    val runSBT = RunSBT(SBTConfig(CommandLineExecutableConfig("/Users/madoc/bin/sbt")))
    val proc = runSBT(new File("/Users/madoc/tmp/testprj"), CompileCommand)
    //proc.outputLines foreach println
    SBTEventParser(proc outputLines) foreach println
  }

  runOnce()
}
