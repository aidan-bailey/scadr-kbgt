package kbgt.generation

import kbgt.logic.Atom

/** A DCG relation. */
sealed abstract class Relation(antecedent: Atom, consequent: Atom)

/** A DCG conflict relation. */
case class ConflictRelation(antecedent: Atom, consequent: Atom)
    extends Relation(antecedent: Atom, consequent: Atom)

/** A DCG defeasible relation. */
case class DefeasibleRelation(antecedent: Atom, consequent: Atom)
    extends Relation(antecedent: Atom, consequent: Atom)
