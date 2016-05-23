package slacc
package generic

import utils._
import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._

/* stage 3.5
  after tree generations, will generate new subtrees based off identifiers
  present in all other forms of trees.
  we then attach as normal using specialized naming scheme that 
  the lexer would normally ignore.
  removes the generic trees as well, since static typing. Therefore, no errors will occur.
  
  TODO: CREATE DIFFERENT SUBTREES WITH EACH OF THE TYPES. THEREFORE, CAN HAVE TYPE OF LINKEDLIST#INT.
  USE # AS AN IDENTIFIER TO DETERMINE THE CLASS TYPE.  ATTACH THE POSITION OF THE CODE AS NORMAL ( SO THAT ERRORS WORK FINE )
*/
object GenericAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    /* Class Decl to -> identifiers */
    var genericMap = Map[ClassDecl,List[Identifier]]();

    /* List of Concrete Class Definitions */
    var concreteList = List[ClassDecl]();

    /* Step 1: Aggregate all generic class uses into the map */

    /* Step 2: Generate all the concrete classes and insert into list */

    /* Step 3: Delete all abstract generic classes from program. */

    /* Step 4: Add all the concrete classes from list into tree */

    /* Done */


    terminateIfErrors
    prog
  }

}
