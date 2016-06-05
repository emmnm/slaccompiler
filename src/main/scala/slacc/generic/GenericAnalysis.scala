package slacc
package generic

import scala.collection.immutable.Queue

import utils._
import ast.Printer
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

    def containsID(v : TypeTree, n:Identifier) : Boolean = {
      v match {
        case v : Identifier =>
          if(v.value == n.value) true
          else v.cast match {
            case Some(v2) => containsID(v2,n)
            case None => false
          }
        case _ => false
        }
      }

    def replaceID(v : TypeTree, old : Identifier, n : TypeTree): TypeTree = {
      v match {
        case v : Identifier =>
          if(v.value == old.value) n.setPos(old)
          else v.cast match {
            case Some(v2) => new Identifier(v.value,Some(replaceID(v2,old,n))).setPos(v)
            case None => v
          }
        case _ => v
      }
    }

    /* Step 1: Aggregate all generic class uses into the map */
    val genericList : List[ClassDecl] = prog.classes.filter( x => x.generic match {case Some(e) => true case None => false } )

    /* Step 1.5) Determine all required subtypes required by scanning the tree for occurances, finding the new keyword and checking the cast of Identifier. */

    def scanTree(tr : Tree, cls : ClassDecl) = {
      var lst = List[TypeTree]()
      def scanTree(t : Tree) : Unit = { // returns the tree.    
          t match {
          case t : Program => t.classes.foreach(scanTree); scanTree(t.main)
          case t : MainMethod => scanTree(t.main)
          case t : ClassDecl => t.vars.foreach(scanTree); t.methods.foreach(scanTree)
          case t : VarDecl => scanTree(t.tpe)
          case t : MethodDecl => scanTree(t.retType); t.args.foreach(scanTree); t.vars.foreach(scanTree); t.exprs.foreach(scanTree); scanTree(t.retExpr)
          case t : Formal => scanTree(t.tpe)
          case t : IntArrayType =>
          case t : IntType =>
          case t : BooleanType =>
          case t : StringType =>
          case t : UnitType =>
          case t : And => scanTree(t.lhs); scanTree(t.rhs)
          case t : Or => scanTree(t.lhs); scanTree(t.rhs)
          case t : Plus => scanTree(t.lhs); scanTree(t.rhs)
          case t : Minus => scanTree(t.lhs); scanTree(t.rhs)
          case t : Times => scanTree(t.lhs); scanTree(t.rhs)
          case t : Div => scanTree(t.lhs); scanTree(t.rhs)
          case t : LessThan => scanTree(t.lhs); scanTree(t.rhs)
          case t : Equals => scanTree(t.lhs); scanTree(t.rhs)
          case t : ArrayRead => scanTree(t.arr); scanTree(t.index)
          case t : ArrayLength => scanTree(t.arr);
          case t : MethodCall => scanTree(t.obj); t.args.foreach(scanTree)
          case t : IntLit =>
          case t : StringLit => 
          case t : True => 
          case t : False => 

          /* Here add to list, and also change the occurance to the new # symbol. */
          case t : Identifier => {
            if(t.value == cls.id.value) {
              t.cast match {
                case Some(v) =>
                  val n = cls.generic.get
                  if(!containsID(v,n)) lst = v :: lst;
                case None => error("Expected Parameterized Type",t)
              }
            }
          }
          case t : NewIntArray => scanTree(t.size)
          case t : New => scanTree(t.tpe)
          case t : Not => scanTree(t.expr)
          case t : Self =>
          case t : Block => t.exprs.foreach(scanTree)
          case t : If => scanTree(t.expr); scanTree(t.thn);
            t.els match { case Some(x) => scanTree(x); case None => }
          case t : While => scanTree(t.cond); scanTree(t.body)
          case t : Println => scanTree(t.expr)
          case t : Assign => scanTree(t.expr)
          case t : ArrayAssign => scanTree(t.index); scanTree(t.expr)
          case t : Strof => scanTree(t.expr)
          case _ => System.err.println("Not implemented yet")
          }
      }
      scanTree(tr)
      lst
    }

    /* Class Decl to -> Types */
    var genM = Map[ClassDecl,List[TypeTree]]();
    genM = genericList.foldLeft(Map[ClassDecl, List[TypeTree]]()) { (m, c) => m + (c -> scanTree(prog,c)) }

    /* Step 2: Generate all the concrete classes and insert into list */


    //converts a generic type tree into an Identifier Type Tree by flattening into a string.

    def tpe2str(tpe : TypeTree ): String = {
    tpe match {
      case t : IntArrayType => "Int[]"
      case t : IntType => "Int"
      case t : BooleanType => "Bool"
      case t : StringType => "String"
      case t : UnitType => "Unit"
      case t : Identifier => t.value + "#" + (if(t.cast.isDefined) tpe2str(t.cast.get) else "")
    }
    }


    def cloneTree(cls : ClassDecl, newTpe : TypeTree) : ClassDecl = {

      val oldTpe = cls.generic.get
      //cls

      def cloneClassDecl(t : ClassDecl) : ClassDecl = {
        ClassDecl(Identifier(t.id.value+"#" + tpe2str(newTpe)).setPos(t.id),
            None,t.parent,t.vars.map(cloneVarDecl),t.methods.map(cloneMethodDecl)).setPos(t)
      }
      def cloneVarDecl(t : VarDecl) : VarDecl = {
        VarDecl(cloneTypeTree(t.tpe),t.id).setPos(t)
      }
      def cloneMethodDecl(t : MethodDecl) : MethodDecl = {
        MethodDecl(cloneTypeTree(t.retType),t.id,t.args.map(cloneFormal),t.vars.map(cloneVarDecl),
            t.exprs.map(cloneExprTree),cloneExprTree(t.retExpr)).setPos(t)
      }
      def cloneFormal(t : Formal) : Formal = {
        Formal(cloneTypeTree(t.tpe),t.id).setPos(t)
      }
      def cloneExprTree(t : ExprTree) : ExprTree = {
        t match {
        case t : And => new And(cloneExprTree(t.lhs),cloneExprTree(t.rhs)).setPos(t)
        case t : Or => new Or(cloneExprTree(t.lhs),cloneExprTree(t.rhs)).setPos(t)
        case t : Plus => new Plus(cloneExprTree(t.lhs),cloneExprTree(t.rhs)).setPos(t)
        case t : Minus => new Minus(cloneExprTree(t.lhs),cloneExprTree(t.rhs)).setPos(t)
        case t : Times => new Times(cloneExprTree(t.lhs),cloneExprTree(t.rhs)).setPos(t)
        case t : Div => new Div(cloneExprTree(t.lhs),cloneExprTree(t.rhs)).setPos(t)
        case t : LessThan => new LessThan(cloneExprTree(t.lhs),cloneExprTree(t.rhs)).setPos(t)
        case t : Equals => new Equals(cloneExprTree(t.lhs),cloneExprTree(t.rhs)).setPos(t)
        case t : ArrayRead => new ArrayRead(cloneExprTree(t.arr),cloneExprTree(t.index)).setPos(t)
        case t : ArrayLength => new ArrayLength(cloneExprTree(t.arr)).setPos(t)
        case t : MethodCall => new MethodCall(cloneExprTree(t.obj),t.meth,t.args.map(cloneExprTree)).setPos(t)
        case t : IntLit => t
        case t : StringLit => t
        case t : True => t
        case t : False => t
        case t : Identifier => if(t.cast.isDefined) { System.err.println("INTERNAL ERROR") }; new Identifier(t.value).setPos(t)
        case t : Self => t
        case t : NewIntArray => new NewIntArray(cloneExprTree(t.size)).setPos(t)
        case t : New => new New(cloneTypeIdentifier(t.tpe)).setPos(t) //t //New(cloneTypeIdentifier(t.tpe)).setPos(t) //UPDATE THIS L8tr for LinkedList[LinkedList[Int]]
        case t : Not => new Not(t.expr).setPos(t)
        case t : Block => new Block(t.exprs.map(cloneExprTree)).setPos(t)
        case t : If => new If(cloneExprTree(t.expr),cloneExprTree(t.thn),t.els match{case Some(e) => Some(cloneExprTree(e)) case None => None }).setPos(t)
        case t : While => new While(cloneExprTree(t.cond),cloneExprTree(t.body)).setPos(t)
        case t : Println => new Println(cloneExprTree(t.expr)).setPos(t)
        case t : Assign => new Assign(new Identifier(t.id.value).setPos(t.id),cloneExprTree(t.expr)).setPos(t)
        case t : ArrayAssign => new ArrayAssign(new Identifier(t.id.value).setPos(t.id),cloneExprTree(t.index),cloneExprTree(t.expr)).setPos(t)
        case t : Strof => new Strof(cloneExprTree(t.expr)).setPos(t)
        }
      }
      def cloneTypeTree(t : TypeTree) : TypeTree = { 
        t match {
          case t : Identifier => if(containsID(t,oldTpe)) { replaceID(t,oldTpe,newTpe) } else t //if(t.value == oldTpe.value) newTpe.setPos(t) else t
          case _ => t
        }
      } //real work here.
      def cloneTypeIdentifier(tyy : Identifier) : Identifier = {
        if(containsID(tyy,oldTpe)) replaceID(tyy,oldTpe,newTpe) match {
          case tyy : Identifier => tyy
          case _ => System.err.println("INTERNAL ERROR"); Identifier("ERROR")
        }
        else tyy
      }

/*      def cloneTree(t : Tree) : Tree = { // returns the tree.    
          t match {
//          case t : Program => t.classes.foreach(cloneTree); cloneTree(t.main)
//          case t : MainMethod => cloneTree(t.main)
          case t : ClassDecl => t.vars.foreach(cloneTree); t.methods.foreach(cloneTree)
          case t : VarDecl => cloneTree(t.tpe)
          case t : MethodDecl => cloneTree(t.retType); t.args.foreach(cloneTree); t.vars.foreach(cloneTree); t.exprs.foreach(cloneTree); cloneTree(t.retExpr)
          case t : Formal => cloneTree(t.tpe)
          case t : IntArrayType =>
          case t : IntType =>
          case t : BooleanType =>
          case t : StringType =>
          case t : UnitType =>
          case t : And => cloneTree(t.lhs); cloneTree(t.rhs)
          case t : Or => cloneTree(t.lhs); cloneTree(t.rhs)
          case t : Plus => cloneTree(t.lhs); cloneTree(t.rhs)
          case t : Minus => cloneTree(t.lhs); cloneTree(t.rhs)
          case t : Times => cloneTree(t.lhs); cloneTree(t.rhs)
          case t : Div => cloneTree(t.lhs); cloneTree(t.rhs)
          case t : LessThan => cloneTree(t.lhs); cloneTree(t.rhs)
          case t : Equals => cloneTree(t.lhs); cloneTree(t.rhs)
          case t : ArrayRead => cloneTree(t.arr); cloneTree(t.index)
          case t : ArrayLength => cloneTree(t.arr);
          case t : MethodCall => cloneTree(t.obj); t.args.foreach(cloneTree)
          case t : IntLit =>
          case t : StringLit => 
          case t : True => 
          case t : False => 

          /* Here add to list, and also change the occurance to the new # symbol. */
          case t : Identifier => {
            if(t.value == cls.id.value) {
              t.cast match {
                case Some(v) =>
                  cls.generic match {
                    case Some(n) => {
                      def containsID(v : TypeTree,n:Identifier) : Boolean = {
                        v match {
                          case v : Identifier =>
                            if(v.value == n.value) true
                            else v.cast match {
                              case Some(v2) => containsID(v2,n)
                              case None => false
                            }
                          case _ => false
                        }
                      }
                      //if(!containsID(v,n)) lst = v :: lst;
                    }
                    case None => System.err.println("Internal Class Error")
                  }
                case None => error("Expected Parameterized Type",t)
              }
            }
          }
          case t : NewIntArray => cloneTree(t.size)
          case t : New => cloneTree(t.tpe)
          case t : Not => cloneTree(t.expr)
          case t : Self =>
          case t : Block => t.exprs.foreach(cloneTree)
          case t : If => cloneTree(t.expr); cloneTree(t.thn);
            t.els match { case Some(x) => cloneTree(x); case None => }
          case t : While => cloneTree(t.cond); cloneTree(t.body)
          case t : Println => cloneTree(t.expr)
          case t : Assign => cloneTree(t.expr)
          case t : ArrayAssign => cloneTree(t.index); cloneTree(t.expr)
          case t : Strof => cloneTree(t.expr)
          case _ => System.err.println("Not implemented yet")
          }
      } */
      cloneClassDecl(cls)
    }

    /* List of Concrete Class Definitions */
    var concreteList = List[ClassDecl]();

    //convert the ClassDecl, List(Types) to a queue of Tuples.
    var workList = List[(ClassDecl,TypeTree)]()       //Queue[(ClassDecl,TypeTree)]()
    var implSet = Set[(ClassDecl,TypeTree)]()

    for((k,v) <- genM) {
      workList = v.map(x => (k,x)) ::: workList
    }

/*
  val q0 = collection.immutable.Queue("1","Two","iii")
  Iterator.iterate(q0) { qi =>
    val (e,q) = qi.dequeue
    println("I just dequeued "+e)  // Your side-effecting operations go here
    if (e.length!=2) q.enqueue("..")  // Your changes to the queue go here
    else q
  }.takeWhile(! _.isEmpty).foreach(identity)
*/
  val q0 = Queue().enqueue(workList)
  Iterator.iterate(q0) { qi =>
    val (e,q) = qi.dequeue
    //side effects.
    if(!(implSet contains e)) {
      val newTree = cloneTree(e._1,e._2)
      concreteList = newTree :: concreteList
      implSet = implSet + e
      val depLst = genericList.foldLeft(List[(ClassDecl,TypeTree)]())( (acc,x) => scanTree(newTree,x).map( y => (x,y) ) ::: acc )
      q.enqueue(depLst)
    } else {
      q
    }
    //scanTree(newTree,classDecl ) for all ClassDecl and then returns a list of types.
    //q.enqueue() //enqueue new list from newTree by scanTree all 
  }.takeWhile(!_.isEmpty).foreach(identity)


    /* Step 3: Delete all abstract generic classes from program. */

    var finClasses = (prog.classes ::: concreteList ).toSet -- genericList;
  

//    println(finClasses -- genericList)
    //finClasses.foreach(x => println(x.id))

    /* Step 4: Add all the concrete classes from list into tree */

    val finProg = Program(prog.main,finClasses.toList);

    /* Done */


    terminateIfErrors
    finProg
  }



}
