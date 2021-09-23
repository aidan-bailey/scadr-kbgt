package kbgt.generation

import kbgt.logic.Atom
import scala.collection._
import scala.collection.mutable.ListBuffer
import scala.util.Random

/** An atom generator.
  *
  * @constructor
  *   create a new atom generator with a set of atoms already in use
  * @param atoms
  *   already in use atoms
  */
class AtomGenerator(atoms: mutable.Set[Atom]) {

  private var generatedAtoms = ListBuffer[Atom]()

  private var index = 0

  def this() = this(mutable.Set[Atom]())

  /** Generate a new atom.
    *
    * @return
    *   the new atom
    */
  def generateAtom(): Atom = {
    var newAtom = Atom(index.toString())
    while (atoms.contains(newAtom) || generatedAtoms.contains(newAtom)) {
      index += 1
      newAtom = Atom(index.toString())
    }
    generatedAtoms += newAtom
    return newAtom
  }

  /** Get previously generated atoms.
    *
    * @return
    *   the previously generated atom list.
    */
  def getAtomList() = generatedAtoms.clone()

}
