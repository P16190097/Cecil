/*
 * CESIL+ program analyser
 * Author: David Smallwood
 * January 2020
 */

package cesil

import cesil.Lang._

/**
 * A redacted version of an original library of code analysis methods (`AnalyserReference`).
 * This version is made available as a template for the associated assignment. The methods
 * to be written as part of the assignment are included in this file and implemented as
 * thrown exceptions. Replace each of the thrown exceptions with alternative code to
 * achieve the required results.
 */
object Analyser {

  /**
    * Selects only the labelled statements in a program. For example
    * {{{
    *          IN
    *          JINEG(L2)
    *   L1     OUT
    *          LINE
    *          JIZERO(L2)
    *          SUBTRACT(Val(1))
    *          JUMP(L1)
    *   L2     PRINT("Done")
    *          HALT
    * }}}
    * would yield
    * {{{
    *   L1     OUT
    *   L2     PRINT("Done")
    * }}}
    *
    * @param statements a program.
    * @return the sublist of labelled statements.
    */
  def filterLabelled(statements: Statements): List[Labelled] =
    (statements filter (_.isLabelled)).asInstanceOf[List[Labelled]]

  /**
    * Selects only the unlabelled statements in a program. For example
    * {{{
    *          IN
    *          JINEG(L2)
    *   L1     OUT
    *          LINE
    *          JIZERO(L2)
    *          SUBTRACT(Val(1))
    *          JUMP(L1)
    *   L2     PRINT("Done")
    *          HALT
    * }}}
    * would yield
    * {{{
    *          IN
    *          JINEG(L2)
    *          LINE
    *          JIZERO(L2)
    *          SUBTRACT(Val(1))
    *          JUMP(L1)
    *          HALT
    * }}}
    *
    * @param statements a program.
    * @return the sublist of labelled statements.
    */
  def filterUnlabelled(statements: Statements): List[Unlabelled] =
    (statements filter (_.isUnlabelled)).asInstanceOf[List[Unlabelled]]

  /**
    * Returns a list of the labels used in a program. Note that
    * each label in a program should only appear once (two statements should
    * not have the same label). However, any duplicates that may (wrongly) be
    * present in the program will still appear in the result. In the following
    * example the label `START` occurs in the result despite being redundant
    * in the program istelf.
    * {{{
    *   START  IN
    *          JINEG(L2)
    *   L1     OUT
    *          LINE
    *          JIZERO(L2)
    *          SUBTRACT(Val(1))
    *          JUMP(L1)
    *   L2     PRINT("Done")
    *          HALT
    * }}}
    * returns `List(L1, L2, START)`.
    *
    * @param statements a program.
    * @return the labels in sorted order.
    */
  def getLabels(statements: Statements): List[Label] =
    (filterLabelled(statements) map (_.label)).sorted

  /**
    * Returns a list of program statements stripped of labels. The resulting
    * list is not (intentionally) useful as a program because label references
    * in jump statements will no longer have labels to match. However, the
    * resulting list can be used as part of some further program analysis.
    * For example, the program
    * {{{
    *          IN
    *          JINEG(L2)
    *   L1     OUT
    *          LINE
    *          JIZERO(L2)
    *          SUBTRACT(Val(1))
    *          JUMP(L1)
    *   L2     PRINT("Done")
    *          HALT
    * }}}
    * becomes
    * {{{
    *          IN
    *          JINEG(L2)
    *          OUT
    *          LINE
    *          JIZERO(L2)
    *          SUBTRACT(Val(1))
    *          JUMP(L1)
    *          PRINT("Done")
    *          HALT
    * }}}
    *
    * @param statements a program.
    * @return the program (in original order) with labels removed.
    */
  def stripLabels(statements: Statements): List[Unlabelled] =
    statements map {
      case Labelled(_, unlabelled) => unlabelled
      case unlabelled: Unlabelled  => unlabelled
    }

  /**
    * Returns an alphabetical list of the statement types used by the program. For
    * example
    * {{{
    *          IN
    *          JINEG(L2)
    *   L1     OUT
    *          LINE
    *          JIZERO(L2)
    *          SUBTRACT(Val(1))
    *          JUMP(L1)
    *   L2     PRINT("Done")
    *          HALT
    * }}}
    * returns
    * `List(HALT, IN, JINEG, JIZERO, JUMP, LINE, OUT, PRINT, SUBTRACT)`.
    *
    * @param statements a program.
    * @return the list of statement type names in alphabetical order without duplicates.
    */
  
  def removeRef(name: String): String = name takeRight 1 match {
    case "$" => name.dropRight(1)
    case _ => name
  }
  
  def listStatementTypesUsed(statements: Statements): List[String] = statements.map(statement => statement match {
    case Labelled(label, statement) => removeRef(statement.getClass.getSimpleName)
    case statement => removeRef(statement.getClass.getSimpleName)
  }).sorted.distinct
  
  /**
    * Returns a list of label references used in a program. The list is
    * sorted and duplicates removed. In the following example the redundant
    * label `START` is not referenced and is thus not included in the result.
    * Both `L1` and `L2` are referenced. Therefore the program
    * {{{
    *   START  IN
    *          JINEG(L2)
    *   L1     OUT
    *          LINE
    *          JIZERO(L2)
    *          SUBTRACT(Val(1))
    *          JUMP(L1)
    *   L2     PRINT("Done")
    *          HALT
    * }}}
    * yields `List(L1, L2)`.  Note that if a program (wrongly) contains
    * references to non-existant labels then these too will be included in
    * the result.
    *
    * @param statements a program.
    * @return the list of unique label references in sorted order.
    */
  
  def filterNulls(x: String): Boolean = x match {
    case null => false
    case _ => true
  }

  def getLabelRefs(statements: Statements): List[Label] = statements.map(statement => statement match {
      case Labelled(label, JUMP(ref)) => ref
      case Labelled(label, JINEG(ref)) => ref
      case Labelled(label, JIPOS(ref)) => ref
      case Labelled(label, JIZERO(ref)) => ref
      case JUMP(ref) => ref
      case JINEG(ref) => ref
      case JIPOS(ref) => ref
      case JIZERO(ref) => ref
      case _ => null
    })
    .filter(filterNulls)
    .sorted
    .distinct


  /**
    * Returns an ordered list of all (redundant) labels in a program. A redundant label
    * is one that has no reference. The resulting list may be empty if all labels in a
    * program are referenced. Therefore the program
    * {{{
    *   START  IN
    *          JINEG(L2)
    *   L1     OUT
    *          LINE
    *          JIZERO(L2)
    *          SUBTRACT(Val(1))
    *          JUMP(L1)
    *   L2     PRINT("Done")
    *          HALT
    * }}}
    * returns `List(START)`.
    *
    * @param statements a program.
    * @return the ordered list of redundant labels.
    */
    
  def redundantLabels(statements: Statements): List[Label] = getLabels(statements).filter(!getLabelRefs(statements).contains(_)).sorted

  /**
    * Returns an ordered list of all missing labels in a program. A missing label
    * is one that is expected by some label reference. The resulting list may be empty
    * if no labels in a program are missing. Therefore the program
    * {{{
    *   START  IN
    *          JINEG(L2)
    *          OUT
    *          LINE
    *          JIZERO(L2)
    *          SUBTRACT(Val(1))
    *          JUMP(L1)
    *   L2     PRINT("Done")
    *          HALT
    * }}}
    * returns `List(L1)`.
    *
    * @param statements a program.
    * @return the ordered list of redundant labels.
    */
  def missingLabels(statements: Statements): List[Label] = getLabelRefs(statements).filter(!getLabels(statements).contains(_)).sorted

  /**
    * Attaches line numbers (indexes) to a program starting with index 0. This method
    * is used by the [[VirtualMachine]] when a program is loaded so that its program
    * counter can step through the statements in turn.
    *
    * @param statements a program.
    * @return a list of (index, statement) pairs.
    */
  def indexProgram(statements: Statements): List[(Int, Statement)] =
    (statements.indices zip statements).toList

  /**
    * Returns a mapping of labels and their respective line numbers.
    * For example, if the following program is input
    * {{{
    *          IN
    *          JINEG(L2)
    *   L1     OUT
    *          LINE
    *          JIZERO(L2)
    *          SUBTRACT(Val(1))
    *          JUMP(L1)
    *   L2     PRINT("Done")
    *          HALT
    * }}}
    * then the mapping generated is: `( L1 -> 2, L2 -> 7 )`. This method is called
    * by the [[VirtualMachine]] when loading and analysing a program.
    *
    * @param statements a program.
    * @return a `Map` associating each label with the index of the line
    *         on which it is defined.
    */
  def makeLabelIndexMap(statements: Statements): Map[Label, Int] =
    (indexProgram(statements) filter (_._2.isLabelled) map {
      case (index, Labelled(label, _)) => label -> index
    }).toMap

  /**
    * Removes all PUSH/POP pairs. A PUSH followed immediately by a POP leaves the
    * machine state exactly as it was before the PUSH. Therefore such pairs of
    * statements can be removed safely. For example, the following program fragment
    * {{{
    *   #30    NOP
    *          LOAD      x
    *          PUSH
    *          POP
    *          STORE     y
    *          LOAD      n
    *          SUBTRACT  y
    * }}}
    * can be reduced to
    * {{{
    *   #30    NOP
    *          LOAD      x
    *          STORE     y
    *          LOAD      n
    *          SUBTRACT  y
    * }}}
    * However, care must be taken if either of the PUSH/POP statements is labelled.
    * If the POP is labelled then the optimisation is not possible (because the POP
    * may be executed after ''any'' statement that references its label). If the PUSH
    * is labelled then the label needs to be preserved following the optimisation.
    * In this case the PUSH/POP pair can be replaced by a labelled NOP. ('''N.B.'''
    * NOPs may be removed subsequently using another optimisation: [[stripNOPs]]).
    * {{{
    *   #30    NOP
    *          LOAD      x
    *   #42    PUSH
    *          POP
    *          STORE     y
    *          LOAD      n
    *          SUBTRACT  y
    * }}}
    * can be reduced to
    * {{{
    *   #30    NOP
    *          LOAD      x
    *   #42    NOP
    *          STORE     y
    *          LOAD      n
    *          SUBTRACT  y
    * }}}
    *
    * @param statements a program.
    * @return the program with unlabelled PUSH/POP pairs removed and any
    *         labelled PUSH/unlabelled POP pairs replaced by a labelled NOP.
    */
    
  
  def stripPushPopPairs(statements: Statements): Statements = statements ++ Nil match {
    case (Labelled(label, PUSH) :: POP :: ys) => Labelled(label, NOP) :: stripPushPopPairs(ys)
    case (PUSH :: POP :: z) => stripPushPopPairs(z)
    case (x :: y :: z) => x :: stripPushPopPairs(y :: z)
    case (statement) => (statement)
    case List() => List()
  }


  /**
    * Removes all unlabelled NOP statements. Such statements can be filtered out without
    * changing the meaning of the program.
    *
    * @param statements a program.
    * @return the program with all unlabelled NOPs removed.
    */
  def stripUnlabelledNOPs(statements: Statements): Statements = statements.filter(statement => statement match {
    case NOP => false
    case statement => true
  })


  /**
    * Removes all labelled NOP statements. A labelled NOP can have its label transferred
    * to the following statement. Then all statements that refer to this label must have
    * their references updated. For example
    * {{{
    *          IN
    *          JINEG(L2)
    *   L1     OUT
    *          LINE
    *          JIZERO(L2)
    *          SUBTRACT(Val(1))
    *          JUMP(L1)
    *   L2     NOP
    *          LOAD(Val(2))
    * }}}
    * In this case the label L2 can be transferred to the subsequent statement:
    * {{{
    *          IN
    *          JINEG(L2)
    *   L1     OUT
    *          LINE
    *          JIZERO(L2)
    *          SUBTRACT(Val(1))
    *          JUMP(L1)
    *   L2     LOAD(Val(2))
    * }}}
    * However, if the subsequent statement is labelled then a relabelling of
    * references is necessary. For example
    * {{{
    *          IN
    *          JINEG(L2)
    *   L1     OUT
    *          LINE
    *          JIZERO(L2)
    *          SUBTRACT(Val(1))
    *          JUMP(L1)
    *   L2     NOP
    *   L3     LOAD(Val(2))
    * }}}
    * In this case the label L2 is redundant and references to L2 need to be changed
    * to refer to L3.
    * {{{
    *          IN
    *          JINEG(L3)
    *   L1     OUT
    *          LINE
    *          JIZERO(L3)
    *          SUBTRACT(Val(1))
    *          JUMP(L1)
    *   L3     LOAD(Val(2))
    * }}}
    * In general, a sequence of labelled NOPs can be handled similarly:
    * {{{
    *          IN
    *          JINEG(L4)
    *   L1     OUT
    *          LINE
    *          JIZERO(L2)
    *          SUBTRACT(Val(1))
    *          JUMP(L1)
    *   L4     NOP
    *   L2     NOP
    *   L3     LOAD(Val(2))
    * }}}
    * In this example any references to L2 or L4 must all be changed to refer
    * to label L3
    * {{{
    *          IN
    *          JINEG(L3)
    *   L1     OUT
    *          LINE
    *          JIZERO(L3)
    *          SUBTRACT(Val(1))
    *          JUMP(L1)
    *   L3     LOAD(Val(2))
    * }}}
    * However, if the very last statement is a labelled NOP then this cannot be removed.
    * (There is no subsequent statement to which to attach the label). In such programs
    * the final statement remains a labelled NOP.
    *
    * @param statements a program.
    * @return a program with the labelled NOPs removed and associated references relabelled.
    */
  
  def stripLabelledNOPs(statements: Statements): Statements = statements ++ Nil match {
    case (Labelled(label, NOP) :: LOAD(ref) :: z) => Labelled(label, LOAD(ref)) :: stripLabelledNOPs(z)
    case (Labelled(label, NOP) :: STORE(ref) :: z) => Labelled(label, STORE(ref)) :: stripLabelledNOPs(z)
    case (Labelled(label, NOP) :: JUMP(ref) :: z) => Labelled(label, JUMP(ref)) :: stripLabelledNOPs(z)
    case (Labelled(label, NOP) :: JINEG(ref) :: z) => Labelled(label, JINEG(ref)) :: stripLabelledNOPs(z)
    case (Labelled(label, NOP) :: JIPOS(ref) :: z) => Labelled(label, JIPOS(ref)) :: stripLabelledNOPs(z)
    case (Labelled(label, NOP) :: JIZERO(ref) :: z) => Labelled(label, JIZERO(ref)) :: stripLabelledNOPs(z)
    case (Labelled(label, NOP) :: PRINT(ref) :: z) => Labelled(label, PRINT(ref)) :: stripLabelledNOPs(z)
    case (Labelled(label, NOP) :: LINE :: z) => Labelled(label, LINE) :: stripLabelledNOPs(z)
    case (Labelled(label, NOP) :: IN :: z) => Labelled(label, IN) :: stripLabelledNOPs(z)
    case (Labelled(label, NOP) :: OUT :: z) => Labelled(label, OUT) :: stripLabelledNOPs(z)
    case (Labelled(label, NOP) :: ADD(ref) :: z) => Labelled(label, ADD(ref)) :: stripLabelledNOPs(z)
    case (Labelled(label, NOP) :: SUBTRACT(ref) :: z) => Labelled(label, SUBTRACT(ref)) :: stripLabelledNOPs(z)
    case (Labelled(label, NOP) :: MULTIPLY(ref) :: z) => Labelled(label, MULTIPLY(ref)) :: stripLabelledNOPs(z)
    case (Labelled(label, NOP) :: DIVIDE(ref) :: z) => Labelled(label, DIVIDE(ref)) :: stripLabelledNOPs(z)
    case (Labelled(label, NOP) :: MODULO(ref) :: z) => Labelled(label, MODULO(ref)) :: stripLabelledNOPs(z)
    case (Labelled(label, NOP) :: PUSH :: z) => Labelled(label, PUSH) :: stripLabelledNOPs(z)
    case (Labelled(label, NOP) :: POP :: z) => Labelled(label, POP) :: stripLabelledNOPs(z)
    case (Labelled(label, NOP) :: SWAP :: z) => Labelled(label, SWAP) :: stripLabelledNOPs(z)
    case (Labelled(label, NOP) :: NOP :: z) => Labelled(label, NOP) :: stripLabelledNOPs(z)
    case (Labelled(label, NOP) :: HALT :: z) => Labelled(label, HALT) :: stripLabelledNOPs(z)
    case (x :: y :: z) => x :: stripLabelledNOPs(y :: z)
    case statement => statement
    case List() => List()
  }


  /**
    * Removes all NOP statements (performing any relabelling that may be needed). This is
    * achieved in two passes: firstly, remove the unlabelled NOPs using [[stripUnlabelledNOPs]];
    * and then remove the labelled NOPs using [[stripLabelledNOPs]]. This function is
    * constructed using functional composition.
    */
  val stripNOPs: Statements => Statements = stripLabelledNOPs _ compose stripUnlabelledNOPs _

  def stripAllPushPopPairs(statements: Statements): Statements = statements match {
    case (w :: Labelled(label, PUSH) :: POP :: z) => stripAllPushPopPairs(w :: Labelled(label, NOP) :: z)
    case (w :: PUSH :: POP :: z) => stripAllPushPopPairs(w :: z)
    case (Labelled(label, PUSH) :: POP :: y) => Labelled(label, NOP) :: stripAllPushPopPairs(y)
    case (PUSH :: POP :: z) => stripAllPushPopPairs(z)
    case (x :: y :: z) => x :: stripAllPushPopPairs(y :: z)
    case (statement) => (statement)
    case List() => List()
  }
    //throw new Exception("stripAllPushPopPairs - replace this thrown exception with an implementation")
  
  /*
   * The function above uses recursion similarly to the original 'stripPushPopPairs' function but instead simply 
   * widens the sliding list view to include the next 3 indexes so that if a PUSH POP combo were to be removed the 
   * previous index remains in view so it too can be pattern matched to see if it created a new PUSH POP combo. 
   * This method however will only work for the first new PUSH POP pair created as a result of an optimisation 
   * and so any subsequent PUSH POP pairs would remain in the list.
   */

}

