package com.github.madoc.runsbt

import com.github.madoc.runsbt.config.ExecutableConfig.CommandLineExecutableConfig
import com.github.madoc.runsbt.config.SBTConfig
import com.github.madoc.runsbt.running.{SBTCommand, SBTProcess}

import scala.sys.process._

// TODO working directory
case class RunSBT(sbtConfig:SBTConfig) {
  def apply(command:SBTCommand):SBTProcess = sbtConfig sbtExecutable match {
    case CommandLineExecutableConfig(executablePath) ⇒
      val processBuilder = Process(Seq(executablePath) ++ (command toStringSeq))
      SBTProcess.BasedOnProcessBuilder(processBuilder)
  }
}
