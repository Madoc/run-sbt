package com.github.madoc.runsbt

import java.io.File

import com.github.madoc.runsbt.config.ExecutableConfig.CommandLineExecutableConfig
import com.github.madoc.runsbt.config.SBTConfig
import com.github.madoc.runsbt.events.SBTEventParser
import com.github.madoc.runsbt.running.SBTCommand._

object TestMe extends App {
  def runOnce() {
    //val command = CleanCommand :+ CompileCommand :+ TestCommand :+ PublishLocalCommand
    val command = CleanCommand :+ CompileCommand
    //val projectFolder = "/Users/madoc/code/own/business-name-inspiration"
    val projectFolder = "/Users/madoc/code/own/gtarpg/scenes"
    val runSBT = RunSBT(SBTConfig(CommandLineExecutableConfig("/Users/madoc/bin/sbt")))
    val proc = runSBT(new File(projectFolder), command)
    SBTEventParser(proc outputLines) foreach println
  }

  runOnce()
}
