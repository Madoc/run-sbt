package com.github.madoc.runsbt.events

sealed trait SBTEvent {
  def isError:Boolean = false
}
object SBTEvent {
  sealed case class Compiling(sourceFileCount:Int, sourceLanguage:String, targetDirectory:String,
    scalaAPIPaths:Seq[String], warningsCount:Int, featureWarningsCount:Int, warnings:Seq[CompileWarning],
    documentableTemplateCount:Int) extends SBTEvent
  sealed case class CompletionLine(status:String, duration:String, completionTime:String) extends SBTEvent {
    override def isError = status == "error"
  }
  object DoneUpdating extends SBTEvent {override def toString = "DoneUpdating"}
  sealed case class LoadingPlugins(pluginPath:String) extends SBTEvent
  sealed case class LoadingProjectDefinition(path:String) extends SBTEvent
  sealed case class ModuleNotFound(module:String, tried:Seq[ModuleNotFound_Tried]) extends SBTEvent {
    override def isError = true
  }
  sealed case class NotAnSBTProject(projectPath:String) extends SBTEvent {
    override def isError = true
  }
  sealed case class Packaging(packagePaths:Seq[String], writtenPaths:Seq[String], scalaAPIPaths:Seq[String],
    moduleDeliveries:Seq[ModuleDeliverySpec], publishings:Seq[ModuleDeliveryPublishing], deliveryIvyPaths:Seq[String],
    featureWarningCount:Int, warningCount:Int, documentableTemplateCount:Int, missingMainClass:Boolean,
    warnings:Seq[PackagingWarning]) extends SBTEvent {
    def isEmpty:Boolean = packagePaths.isEmpty && writtenPaths.isEmpty && scalaAPIPaths.isEmpty &&
      moduleDeliveries.isEmpty && publishings.isEmpty && deliveryIvyPaths.isEmpty && featureWarningCount==0 &&
      warningCount==0 && documentableTemplateCount==0 && missingMainClass==false && warnings.isEmpty
    def :+(that:Packaging):Packaging = Packaging(packagePaths++that.packagePaths, writtenPaths++that.writtenPaths,
      scalaAPIPaths++that.scalaAPIPaths, moduleDeliveries++that.moduleDeliveries, publishings++that.publishings,
      deliveryIvyPaths++that.deliveryIvyPaths, featureWarningCount+that.featureWarningCount,
      warningCount+that.warningCount, documentableTemplateCount+that.documentableTemplateCount,
      missingMainClass||that.missingMainClass, warnings++that.warnings)
  }
  object Packaging {
    val empty:Packaging = Packaging(Seq(), Seq(), Seq(), Seq(), Seq(), Seq(), 0, 0, 0, false, Seq())
  }
  sealed case class Resolving(dependency:String) extends SBTEvent
  sealed case class Running(mainClass:String, output:Seq[String]) extends SBTEvent
  sealed case class SetProject(projectName:String, buildPath:String) extends SBTEvent
  sealed case class StageError(stage:String, errorMessage:String) extends SBTEvent {
    override def isError = true
  }
  sealed case class UnitTestEvent(testOutput:Seq[String], runDuration:String, numberOfTestsRun:Int, testsSucceeded:Int,
    testsFailed:Int, testsCanceled:Int, testsIgnored:Int, testsPending:Int, suitesCompletedCount:Int,
    suitesAbortedCount:Int=0) extends SBTEvent
  sealed case class UnrecognizedEvent(line:String) extends SBTEvent {
    override def isError = line startsWith "[error] "
  }
  sealed case class UnresolvedDependencies(dependencies:Seq[String], stackTrace:Seq[String]) extends SBTEvent {
    override def isError = true
  }
  sealed case class Updating(pathDescription:String) extends SBTEvent

  case class CompileWarning(sourceFile:String, lineNumber:Int, message:String, codeExcerpt:Option[String], column:Option[Int])
  case class ModuleDeliveryPublishing(what:String, targetPath:String)
  case class ModuleDeliverySpec(module:String, version:String, context:String, timestamp:String)
  case class ModuleNotFound_Tried(scope:String, uris:Seq[String])
  case class PackagingWarning(message:Seq[String])
}
