package com.github.madoc.runsbt

import com.github.madoc.runsbt.config.SBTConfig
import com.github.madoc.runsbt.running.{SBTCommand, SBTProcess}

case class RunSBT(sbtConfig:SBTConfig) {
  def apply(command:SBTCommand):SBTProcess = ??? // TODO
}
