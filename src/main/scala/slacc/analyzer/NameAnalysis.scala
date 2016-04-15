package slacc
package analyzer

import utils._
import ast.Trees._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check all constraints

    // Step 1: Implementation - Do a search into the tree.
    var gs = new GlobalScope;

    //-----------------------------------------------------
    // CREATE ALL POTENTIAL SYMBOLS (MIGHT BE DUPLICATED)

    def createMainSymbol( p : Program ) : Unit = {
      p.main.setSymbol(new ClassSymbol("Main").setPos(p.main))
      createMethodSymbol( p.main.main, p.main.getSymbol )
      p.classes.foreach( createClassSymbol )
    }
    def createClassSymbol( c : ClassDecl) : Unit = {
      c.setSymbol(new ClassSymbol(c.id.value).setPos(c) )
      c.vars.foreach ( createVarSymbol )
      c.methods.foreach( m => createMethodSymbol(m,c.getSymbol))
    }
    def createVarSymbol( v : VarDecl) : Unit = {
      v.setSymbol(new VariableSymbol(v.id.value).setPos(v))
    }
    def createMethodSymbol( m : MethodDecl, c : ClassSymbol ) {
      m.setSymbol(new MethodSymbol(m.id.value,c).setPos(m))
      m.args.foreach( createFormalSymbol )
      m.vars.foreach( createVarSymbol )
      m.retType match {
        case r : Identifier => r.setSymbol(new VariableSymbol(r.value).setPos(r))
        case _ =>
      }
    }
    def createFormalSymbol( f : Formal ) {
      f.setSymbol( new VariableSymbol( f.id.value ).setPos(f) )
    }

    //-----------------------------------------------------
    // CREATE SYMBOL TABLES / REDUCE / DETECT DUPLICATES
    // ALL POTENTIAL SYMBOLS ARE DEFINED SO NONE SHOULD BE EMPTY / NIL.

    def sort_classes( c1 : ClassDecl, c2 : ClassDecl ) = {
      c1.parent match {
        case Some(par) => (par.value != c2.id.value)
        case None => (true)
      }
    }

    def analyzeMainSymbol( p : Program ) : Unit = {
      gs.mainClass = p.main.getSymbol
      val topological = p.classes.sortWith(sort_classes); //println(topological.map( _.id.value ))
      topological.foreach( analyzeClassSymbol )
      analyzeMethodClassSymbol( p.main.main, p.main.getSymbol )
    }

    def analyzeClassSymbol( c : ClassDecl ) : Unit = {
      //check duplicates.
      gs.lookupClass(c.id.value) match {
        case Some(a) => error("class " + c.id.value + " already defined at " + a.position,c)
        case None => gs.classes += (c.id.value -> c.getSymbol)
      }

      c.id.setSymbol(c.getSymbol) //id tag gets the symbol. 

      //load parent data. if not defined we have cycle. quit.
      c.getSymbol.parent = 
        c.parent match {
          case Some(par) => {
            gs.lookupClass(par.value) match {
              case Some(parsymbol) => par.setSymbol(parsymbol); Some(parsymbol);
              case None => error("Class Cycle Detected for " + c.id.value
                    + " and " + par.value + ".",c); None
            }
          }
          case None => None
        }
      
      //fill values of class. fill in member fields, then the methods.
      c.vars.foreach( x => analyzeVarClassSymbol(x, c.getSymbol) )
      c.methods.foreach( x => analyzeMethodClassSymbol(x, c.getSymbol) )
    }

    def analyzeVarClassSymbol( v : VarDecl, c : ClassSymbol ) : Unit = {
      //will detect errors if defined more than once or member is overloaded.
      c.lookupVar(v.id.value) match {
        case Some(a) => error("var " + v.id.value + " already defined at " + a.position,v)
        case None => c.members += (v.id.value -> v.getSymbol); v.id.setSymbol(v.getSymbol)
      }
    }

    def analyzeMethodClassSymbol( m : MethodDecl, c : ClassSymbol ) {

      //if already defined, check more, else just move ahead.
      c.lookupMethod(m.id.value) match {
        case None => c.methods += (m.id.value -> m.getSymbol); m.id.setSymbol(m.getSymbol)
        case Some(d) => {
          //if the conflict is not in current class, and has same # arguments, accept.
          //otherwise, throw error.
          if(!(c.methods contains m.id.value) && (d.argList.length == m.args.length)) {
            m.getSymbol.overridden = Some(d)
            c.methods += (m.id.value -> m.getSymbol)
            m.id.setSymbol(m.getSymbol)
          } else {
            error("method " + m.id.value + " already defined at " + d.position,m)
          }

        }
      }
      //link internals to method symbol. Start with arguments.
      m.args.foreach ( x => analyzeFormalMethodSymbol(x,m.getSymbol) )
      m.vars.foreach ( x => analyzeVarMethodSymbol(x,m.getSymbol) )
      (m.exprs :+ m.retExpr).foreach ( x => analyzeExprTreeMethod(x,m.getSymbol) )

    }
    def analyzeFormalMethodSymbol( f : Formal , m : MethodSymbol ) {
      m.lookupVar(f.id.value) match {
        case None => m.params += (f.id.value -> f.getSymbol)
        case Some(a) =>
          m.classSymbol.lookupVar(f.id.value) match {
            case Some(b) => m.params += (f.id.value -> f.getSymbol); f.id.setSymbol(f.getSymbol)
            case None => error("parameter " + f.id.value + " already defined at " + a.position,f)
          }
        }
    }

    def analyzeVarMethodSymbol( v : VarDecl, m : MethodSymbol ) {
      m.lookupVar(v.id.value) match {
        case None => m.members += (v.id.value -> v.getSymbol)
        case Some(a) =>
          m.classSymbol.lookupVar(v.id.value) match {
            case Some(b) => m.params += (v.id.value -> v.getSymbol); v.id.setSymbol(v.getSymbol)
            case None => error("var " + v.id.value + " already defined at " + a.position,v)
          }
        }
    }

    def analyzeExprTreeMethod( t : ExprTree, m : MethodSymbol ) {
      //everything defined. if not, throw error.
      t match {
        case id : Identifier => m.lookupVar(id.value) match {
          case Some(sym) => id.setSymbol(sym)
          case None => error(id.value + " not declared.", t)
        }
        case s  : Self => s.setSymbol(m.classSymbol)

        case e : New => gs.lookupClass(e.tpe.value) match {
          case Some(sym) => e.tpe.setSymbol(sym)
          case None => error("class " + e.tpe.value + " not declared.",t)
        }

        case e : MethodCall => analyzeExprTreeMethod(e.obj,m);
          //analyzeExprTreeMethod(e.meth,m); //what does this do? THIS STAYS UNRESOLVED.
          e.args.foreach( x => analyzeExprTreeMethod(x,m) )

        case e : Assign => m.lookupVar(e.id.value) match {
          case Some(sym) => e.id.setSymbol(sym)
          case None => error("var " + e.id.value + " not declared.",t)
        }
          analyzeExprTreeMethod(e.expr,m);

        case e : ArrayAssign => m.lookupVar(e.id.value) match {
          case Some(sym) => e.id.setSymbol(sym)
          case None => error("var " + e.id.value + " not declared.",t)
        } 
          analyzeExprTreeMethod(e.index,m); analyzeExprTreeMethod(e.expr,m);

        /* recursive cases below */
        case e : And => analyzeExprTreeMethod(e.lhs,m); analyzeExprTreeMethod(e.rhs,m);
        case e : Or => analyzeExprTreeMethod(e.lhs,m); analyzeExprTreeMethod(e.rhs,m);
        case e : Plus => analyzeExprTreeMethod(e.lhs,m); analyzeExprTreeMethod(e.rhs,m);
        case e : Minus => analyzeExprTreeMethod(e.lhs,m); analyzeExprTreeMethod(e.rhs,m);
        case e : Times => analyzeExprTreeMethod(e.lhs,m); analyzeExprTreeMethod(e.rhs,m);
        case e : Div => analyzeExprTreeMethod(e.lhs,m); analyzeExprTreeMethod(e.rhs,m);
        case e : LessThan => analyzeExprTreeMethod(e.lhs,m); analyzeExprTreeMethod(e.rhs,m);
        case e : Equals => analyzeExprTreeMethod(e.lhs,m); analyzeExprTreeMethod(e.rhs,m);
        case e : ArrayRead => analyzeExprTreeMethod(e.arr,m); analyzeExprTreeMethod(e.index,m);
        case e : ArrayLength => analyzeExprTreeMethod(e.arr,m);
        case e : NewIntArray => analyzeExprTreeMethod(e.size,m);
        case e : Not => analyzeExprTreeMethod(e.expr,m);
        case e : Block => e.exprs.foreach( x => analyzeExprTreeMethod(x,m));
        case e : If => analyzeExprTreeMethod(e.expr,m); analyzeExprTreeMethod(e.thn,m);
          e.els match { case Some(els) => analyzeExprTreeMethod(els,m) case None => }
        case e : While => analyzeExprTreeMethod(e.cond,m); analyzeExprTreeMethod(e.body,m);
        case e : Println => analyzeExprTreeMethod(e.expr,m);
        case e : Strof => analyzeExprTreeMethod(e.expr,m);

        case _ => //do nothing. Literal.
      }

    }

/*
  case class MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) extends ExprTree

  case class Identifier(value: String) extends TypeTree with ExprTree with Symbolic[Symbol]
  case class Self() extends ExprTree with Symbolic[ClassSymbol]
  case class NewIntArray(size: ExprTree) extends ExprTree
  case class New(tpe: Identifier) extends ExprTree
  case class Not(expr: ExprTree) extends ExprTree

  case class Block(exprs: List[ExprTree]) extends ExprTree
  case class If(expr: ExprTree, thn: ExprTree, els: Option[ExprTree]) extends ExprTree
  case class While(cond: ExprTree, body: ExprTree) extends ExprTree
  case class Println(expr: ExprTree) extends ExprTree
  case class Assign(id: Identifier, expr: ExprTree) extends ExprTree
  case class ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) extends ExprTree
  case class Strof(expr: ExprTree) extends ExprTree
*/

    createMainSymbol(prog)
    analyzeMainSymbol(prog)

    terminateIfErrors
    prog
  }

}
