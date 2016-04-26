package slacc
package analyzer

import utils._
import ast.Trees._
import Symbols._
import Types._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check all constraints

    var gs = new GlobalScope;

    //-----------------------------------------------------
    // CREATE ALL POTENTIAL SYMBOLS (MIGHT BE DUPLICATED)

    def createMainSymbol( p : Program ) : Unit = {
      p.main.setSymbol(new ClassSymbol("Main").setPos(p.main))
      createMethodSymbol( p.main.main, p.main.getSymbol )
      p.classes.foreach( createClassSymbol )
    }
    def createClassSymbol( c : ClassDecl) : Unit = {
      c.setSymbol(new ClassSymbol(c.id.value).setPos(c))
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
/*      m.retType match {
        case r : Identifier => r.setSymbol(new VariableSymbol(r.value).setPos(r))
        case _ => //what do here.
      } */
//      m.getSymbol.argList = m.args.map( f => f.getSymbol )
    }
    def createFormalSymbol( f : Formal ) {
      f.setSymbol( new VariableSymbol( f.id.value ).setPos(f))
    }

    //-----------------------------------------------------
    // CREATE SYMBOL TABLES / REDUCE / DETECT DUPLICATES
    // ALL POTENTIAL SYMBOLS ARE DEFINED SO NONE SHOULD BE EMPTY / NIL.\
    // HELPER METHODS BELOW FOR THE ANALYIZATION STAGE.

    //TYPES ARE NON UNIQUE. CREATE AT WILL.

    /* Given a TypeTree, will attach the Type to the type tree, as well as return the Type
       so that we can attach to the Symbol that it is requested for.
       prereq: The class symbols are all defined. */
    def attachType(tpe : TypeTree) : Type = {
      tpe match {
        case tpe: IntType => tpe.setType(TInt)
        case tpe: BooleanType => tpe.setType(TBoolean)
        case tpe: IntArrayType => tpe.setType(TIntArray)
        case tpe: StringType => tpe.setType(TString)
        case tpe: UnitType => tpe.setType(TUnit)
        case id: Identifier => gs.lookupClass(id.value) match {
            case Some(c) => tpe.setType(c.getType)
            case None => error("class type " + id.value + " not found.",id); tpe.setType(TError)
          }
      }
      tpe.getType
    }

    def sort_classes( c1 : ClassDecl, c2 : ClassDecl ) = {
      c1.parent match {
        case Some(par) => (par.value != c2.id.value)
        case None => (true)
      }
    }

    //-----------------------------------------------------
    // CREATE SYMBOL TABLES / REDUCE / DETECT DUPLICATES
    // ALL POTENTIAL SYMBOLS ARE DEFINED SO NONE SHOULD BE EMPTY / NIL.

    def analyzeMainSymbol( p : Program ) : Unit = {
      gs.mainClass = p.main.getSymbol
      val topological = p.classes.sortWith(sort_classes);
      topological.foreach( analyzeClassSymbol )
      topological.foreach( analyzeClassValues )
      analyzeMethodClassSymbol( p.main.main, p.main.getSymbol ) // TODO: make sure no args + Unit.
    }

    def analyzeClassSymbol( c : ClassDecl ) : Unit = {

      gs.lookupClass(c.id.value) match { //check duplicates.
        case Some(a) => error("class " + c.id.value + " already defined at " + a.position,c)
        case None => c.getSymbol.setType(TObject(c.getSymbol));
          gs.classes += (c.id.value -> c.getSymbol)
      }

      c.id.setSymbol(c.getSymbol) //id tag gets the symbol. 

      c.getSymbol.parent = //load parent data. if not defined we have cycle. quit.
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
    }

    //fill values of class. fill in member fields, then the methods.
    def analyzeClassValues( c : ClassDecl ) : Unit = {
      c.vars.foreach( x => analyzeVarClassSymbol(x, c.getSymbol) )
      c.methods.foreach( x => analyzeMethodClassSymbol(x, c.getSymbol) )
    }

    def analyzeVarClassSymbol( v : VarDecl, c : ClassSymbol ) : Unit = {
      //will detect errors if defined more than once or member is overloaded.
      c.lookupVar(v.id.value) match {
        case Some(a) => error("var " + v.id.value + " already defined at " + a.position,v)
        case None => v.getSymbol.setType(attachType(v.tpe))
          c.members += (v.id.value -> v.getSymbol); v.id.setSymbol(v.getSymbol)
      }
    }

    // TODO: FORCE OVERRIDING CONSTRAINT ON METHOD. ALSO CHECK FOR ARGLIST.
    def analyzeMethodClassSymbol( m : MethodDecl, c : ClassSymbol ) { 
      var addToMap = false;
      //if already defined, check more, else just move ahead.
      c.lookupMethod(m.id.value) match {
        case None => addToMap = true
        case Some(d) => {
          //if the conflict is not in current class, and has same # arguments, accept.
          //otherwise, throw error.
          if(!(c.methods contains m.id.value) && (d.argList.length == m.args.length)) {
            m.getSymbol.overridden = Some(d)
            addToMap = true
          } else {
            error("method " + m.id.value + " already defined at " + d.position,m)
          }
        }
      }

      if(addToMap) {
        c.methods += (m.id.value -> m.getSymbol); m.id.setSymbol(m.getSymbol);
        m.getSymbol.setType(attachType(m.retType))
      }

      //link internals to method symbol. Start with arguments.
      m.args.foreach ( x => analyzeFormalMethodSymbol(x,m.getSymbol) )
      m.getSymbol.argList = m.args.map(x => m.getSymbol.lookupVar(x.id.value) ).flatten
      m.vars.foreach ( x => analyzeVarMethodSymbol(x,m.getSymbol) )
      (m.exprs :+ m.retExpr).foreach ( x => analyzeExprTreeMethod(x,m.getSymbol) )

    }
    def analyzeFormalMethodSymbol( f : Formal , m : MethodSymbol ) {
      var addToMap = false
      m.lookupVar(f.id.value) match {
        case None => addToMap = true; //m.params += (f.id.value -> f.getSymbol)
        case Some(a) =>
          m.classSymbol.lookupVar(f.id.value) match {
            case Some(b) => addToMap = true 
            case None => error("parameter " + f.id.value + " already defined at " + a.position,f)
          }
        }
      if(addToMap) {
        m.params += (f.id.value -> f.getSymbol); f.id.setSymbol(f.getSymbol)
        f.getSymbol.setType(attachType(f.tpe))
      }
    }

    def analyzeVarMethodSymbol( v : VarDecl, m : MethodSymbol ) {
      var addToMap = false;
      m.lookupVar(v.id.value) match {
        case None => addToMap = true
        case Some(a) =>
          m.classSymbol.lookupVar(v.id.value) match {
            case Some(b) => addToMap = true
            case None => error("var " + v.id.value + " already defined at " + a.position,v)
          }
        }
      if(addToMap) {
        m.params += (v.id.value -> v.getSymbol); v.id.setSymbol(v.getSymbol)
        v.getSymbol.setType(attachType(v.tpe))
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

    createMainSymbol(prog)
    analyzeMainSymbol(prog)

    terminateIfErrors
    prog
  }

}
