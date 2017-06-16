package com.github.madoc.runsbt

import java.io.File

import com.github.madoc.runsbt.config.ExecutableConfig.CommandLineExecutableConfig
import com.github.madoc.runsbt.config.SBTConfig
import com.github.madoc.runsbt.running.{SBTCommand, SBTProcess}

import scala.sys.process._

trait RunSBT {
  def apply(workingDirectory:File, command:SBTCommand):SBTProcess
}
object RunSBT {
  def apply(sbtConfig:SBTConfig):RunSBT = DefaultImpl(sbtConfig)

  case class DefaultImpl(sbtConfig:SBTConfig) extends RunSBT {
    def apply(workingDirectory:File, command:SBTCommand):SBTProcess = sbtConfig sbtExecutable match {
      case CommandLineExecutableConfig(executablePath) ⇒
        val processBuilder = Process(Seq(executablePath) ++ (command toStringSeq), Some(workingDirectory))
        SBTProcess.BasedOnProcessBuilder(processBuilder)
    }
  }
}
