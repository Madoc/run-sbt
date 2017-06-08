package com.github.madoc.runsbt

import com.github.madoc.runsbt.config.ExecutableConfig.CommandLineExecutableConfig
import com.github.madoc.runsbt.config.SBTConfig
import com.github.madoc.runsbt.running.SBTCommand

object TestMe extends App {
  def runOnce() {
    val proc = RunSBT(SBTConfig(CommandLineExecutableConfig("/Users/madoc/bin/sbt")))(SBTCommand.FreeForm("-h"))
    proc.outputLines foreach println
  }

  runOnce()
}
