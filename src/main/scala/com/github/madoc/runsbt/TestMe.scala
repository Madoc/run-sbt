package com.github.madoc.runsbt

import com.github.madoc.runsbt.config.ExecutableConfig.CommandLineExecutableConfig
import com.github.madoc.runsbt.config.SBTConfig
import com.github.madoc.runsbt.running.SBTCommand

object TestMe extends App {
  def runOnce() {
    val runSBT = RunSBT(SBTConfig(CommandLineExecutableConfig("/Users/madoc/bin/sbt")))
    val proc = runSBT(SBTCommand.FreeForm("-h"))
    proc.outputLines foreach println
  }

  runOnce()
}
