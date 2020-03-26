package cesil

import Analyser._
import Lang._

object demo {
  def main(args: Array[String]): Unit = {
    var demoList = List(JUMP("one"))
    
    val oneRef: Statements = List(JUMP("one"))
    val twoRefs: Statements = List(JINEG("two"), JUMP("one"))
    val threeRefs: Statements = twoRefs ++ List(IN, JIZERO("one"), JINEG("three"), JIPOS("two")) ++ oneRef
    val mixedRefs: Statements = List(Labelled("one", JUMP("two")), JUMP("one"), IN, JINEG("three"))
    
    val simple4: Statements = List(Labelled("one",NOP), NOP)

    //println(getLabelRefs(threeRefs))
    //println(listStatementTypesUsed(simple4))
    
    val threeRedundantLabels: Statements = List(Labelled("start", IN), Labelled("one", PUSH), Labelled("two", IN),
      PUSH, Labelled("three", IN), Labelled("four", JINEG("start")), POP, JUMP("three"), OUT, HALT)
    //println(redundantLabels(threeRedundantLabels))
    
    val oneMissingLabel: Statements = List(Labelled("one", JINEG("two")), JUMP("one"), IN, Labelled("two", JINEG("three")))
    val twoMissingLabels: Statements = List(Labelled("three", IN), JINEG("one"), JIPOS("two"), JUMP("three"))
    //println(missingLabels(twoMissingLabels))
    
    val simple1: Statements = List(Labelled("one",NOP))
    //printf(stripPushPopPairs(List()).toString) 
    
    val simpleD: Statements = List(PUSH, POP, Labelled("one",NOP), OUT)
    //printf(stripLabelledNOPs(simpleD).toString)
    
    val simplePairs: Statements = List(PUSH, PUSH, POP, POP)
    println(stripPushPopPairs(simplePairs).toString)
    println(stripAllPushPopPairs(simplePairs).toString)
    println(stripAllPushPopPairs(List(PUSH, POP)).toString)
    println(stripAllPushPopPairs(List(PUSH, PUSH, PUSH, POP, POP, POP)).toString)
  }
}