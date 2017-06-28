package com.github.madoc.runsbt

import java.io.File

import com.github.madoc.runsbt.config.ExecutableConfig.CommandLineExecutableConfig
import com.github.madoc.runsbt.config.SBTConfig
import com.github.madoc.runsbt.events.SBTEvent.Packaging
import com.github.madoc.runsbt.events.SBTEventParser
import com.github.madoc.runsbt.running.SBTCommand._

object TestMe extends App {
  def runOnce() {
    val command = CleanCommand :+ AssemblyCommand :+ PublishLocalCommand
    val projectFolder = "/Users/madoc/code/own/tilde-ci"
    val runSBT = RunSBT(SBTConfig(CommandLineExecutableConfig("/Users/madoc/bin/sbt")))
    val proc = runSBT(new File(projectFolder), command)
    SBTEventParser(proc outputLines) foreach println
    println("-----")
    val packaging = proc.events.foldLeft(Packaging empty) {
      case (acc,p:Packaging) ⇒ acc :+ p
      case (acc,_) ⇒ acc
    }
    println("Packaging:")

    printList("Package paths", packaging packagePaths)
    printList("Written paths", packaging writtenPaths)
    printList("Scala API paths", packaging scalaAPIPaths)
    printList("Module deliveries", packaging moduleDeliveries)
    printList("Publishings", packaging publishings)
    printList("Delivery Ivy paths", packaging deliveryIvyPaths)
    printList("Warnings", packaging warnings)
    println(s"  Feature warning count: ${packaging featureWarningCount}")
    println(s"  Warning count: ${packaging warningCount}")
    println(s"  Documentable template count: ${packaging documentableTemplateCount}")
    println(s"  Missing main class: ${packaging missingMainClass}")

    def printList(name:String, list:Iterable[Any]) {
      println()
      println(s"  $name:")
      list foreach {item ⇒ println(s"  - $item")}
    }
  }

  runOnce()
}
