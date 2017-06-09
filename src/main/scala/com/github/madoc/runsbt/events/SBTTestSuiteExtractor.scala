package com.github.madoc.runsbt.events

import com.github.madoc.runsbt.events.SBTEvent.{UnitTestEvent, UnrecognizedEvent}

private object SBTTestSuiteExtractor extends (Stream[SBTEvent]⇒Stream[SBTEvent]) {
  final def apply(in:Stream[SBTEvent]):Stream[SBTEvent] = in match {
    case (ue@UnrecognizedEvent(_)) #:: in2 ⇒ unrecognizedEventState(ue, in2)
    case head #:: tail ⇒ head #:: apply(tail)
    case Stream.Empty ⇒ Stream.Empty
  }

  private def unrecognizedEventState(ue:UnrecognizedEvent, in:Stream[SBTEvent], events:Vector[UnrecognizedEvent]=Vector()):Stream[SBTEvent] =
    if(signifiesTestEnd(ue)) processTest((events :+ ue) map {_ line} toList) #:: apply(in)
    else in match {
      case (ue2@UnrecognizedEvent(_)) #:: in2 ⇒ unrecognizedEventState(ue2, in2, events :+ ue)
      case _ ⇒ (events :+ ue).toStream ++ apply(in)
    }

  private val testEnds = Seq(
    """\[info\] No tests were executed.""".r.pattern,
    """\[info\] All tests passed.""".r.pattern
  )
  private def signifiesTestEnd(ue:UnrecognizedEvent):Boolean = testEnds exists {_.matcher(ue line) matches}

  private val Pignored1 = """\[warn\] javaOptions will be ignored, fork is set to false""".r
  private val Pignored2 = """\[info\] ScalaTest""".r
  private val Pignored3 = """\[info\] All tests passed.""".r
  private val Pignored4 = """\[info\] No tests were executed.""".r
  private val PnumberOfTestsRun = """\[info\] Total number of tests run: (\d+)""".r
  private val PoutputLineWithLogLevel = """\[\w+\] (.*)""".r
  private val PrunCompleted = """\[info\] Run completed in (.*).""".r
  private val Psuites = """\[info\] Suites: completed (\d+), aborted (\d+)""".r
  private val PtestCounts = """\[info\] Tests: succeeded (\d+), failed (\d+), canceled (\d+), ignored (\d+), pending (\d+)""".r
  private def processTest(in:List[String], _testOutput:Seq[String]=Seq(), _runDuration:String="",
    _numberOfTestsRun:Int=0, _testsSucceeded:Int=0, _testsFailed:Int=0, _testsCanceled:Int=0, _testsIgnored:Int=0,
    _testsPending:Int=0, _suitesCompletedCount:Int=0, _suitesAbortedCount:Int=0):UnitTestEvent = {

    def recurse(in2:List[String], testOutput:Seq[String]=_testOutput, runDuraton:String=_runDuration,
      numberOfTestsRun:Int=_numberOfTestsRun, testsSucceeded:Int=_testsSucceeded, testsFailed:Int=_testsFailed,
      testsCanceled:Int=_testsCanceled, testsIgnored:Int=_testsIgnored, testsPending:Int=_testsPending,
      suitesCompletedCount:Int=_suitesAbortedCount, suitesAbortedCount:Int=_suitesAbortedCount):UnitTestEvent =

      processTest(in2, _testOutput=testOutput, _runDuration=runDuraton, _numberOfTestsRun=numberOfTestsRun,
        _testsSucceeded=testsSucceeded, _testsFailed=testsFailed, _testsCanceled=testsCanceled,
        _testsIgnored=testsIgnored, _testsPending=testsPending, _suitesCompletedCount=suitesCompletedCount,
        _suitesAbortedCount=suitesAbortedCount)

    in match {
      case (Pignored1() | Pignored2() | Pignored3() | Pignored4()) :: in2 ⇒ recurse(in2)
      case PnumberOfTestsRun(num) :: in2 ⇒ recurse(in2, numberOfTestsRun = num toInt)
      case PrunCompleted(duration) :: in2 ⇒ recurse(in2, runDuraton = duration)
      case Psuites(completed, aborted) :: in2 ⇒
        recurse(in2, suitesCompletedCount=completed toInt, suitesAbortedCount=aborted toInt)
      case PtestCounts(succ,fail,canc,ign,pend) :: in2 ⇒
        recurse(in2, testsSucceeded=succ toInt, testsFailed=fail toInt, testsCanceled=canc toInt, testsIgnored=ign toInt, testsPending=pend toInt)

      case PoutputLineWithLogLevel(out) :: in2 ⇒ recurse(in2, testOutput = _testOutput :+ out)
      case anyOtherLine :: in2 ⇒ recurse(in2, testOutput = _testOutput :+ anyOtherLine)
      case List() ⇒ UnitTestEvent(
        testOutput = _testOutput,
        runDuration = _runDuration,
        numberOfTestsRun = _numberOfTestsRun,
        testsSucceeded = _testsSucceeded,
        testsFailed = _testsFailed,
        testsCanceled = _testsCanceled,
        testsIgnored = _testsIgnored,
        testsPending = _testsPending,
        suitesCompletedCount = _suitesCompletedCount,
        suitesAbortedCount = _suitesAbortedCount
      )
    }
  }
}
