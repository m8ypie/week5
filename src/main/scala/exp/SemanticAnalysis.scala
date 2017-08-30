/**
 * Semantic analysis for the Expression language.
 *
 * Copyright 2009-2012, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

import ExpTree.ExpTree
import org.bitbucket.inkytonik.kiama.attribution.Attribution

class SemanticAnalysis (tree : ExpTree) extends Attribution {

    import ExpTree._

    /**
     * Is an expression constant?
     */
    val isconst : Expression => Boolean =
        attr {
            case _:IdnExp => false
            case _:IntExp => true
            case e : PlusExp => isconst(e.left) && isconst(e.left)
            case e : MinusExp => isconst(e.left) && isconst(e.left)
            case e : SlashExp => isconst(e.left) && isconst(e.left)
            case e : StarExp => isconst(e.left) && isconst(e.left)
        }

    /**
     * What is the value of an expression?  Only needs to be valid if the
     * expression is constant (see isconst above).
     */
    val expvalue : Expression => Int =
        attr {
            case int:IntExp => int.num
            case e : PlusExp => expvalue(e.left) + expvalue(e.right)
            case e : MinusExp => expvalue(e.left) - expvalue(e.right)
            case e : SlashExp => expvalue(e.left) / expvalue(e.right)
            case e : StarExp => expvalue(e.left) * expvalue(e.right)
            case _ => 0
        }

}
