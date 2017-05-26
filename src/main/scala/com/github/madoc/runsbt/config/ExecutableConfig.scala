package com.github.madoc.runsbt.config

sealed trait ExecutableConfig
object ExecutableConfig {
  sealed case class CommandLineExecutableConfig(path:String) extends ExecutableConfig
}
