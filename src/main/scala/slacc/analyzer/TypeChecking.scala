package slacc
package analyzer

import ast.Trees._

import Symbols._
import Types._
import utils._

object TypeChecking extends Pipeline[Program, Program] {

  //TODO: Make sure main method has no args and has Unit return type.!

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def tcExpr(expr: ExprTree, expected: Type*): Type = {

      var err_str = "";

      val tpe: Type = expr match {

        case expr : And => tcExpr(expr.lhs,TBoolean); tcExpr(expr.rhs,TBoolean); TBoolean
        case expr : Or => tcExpr(expr.lhs,TBoolean); tcExpr(expr.rhs,TBoolean); TBoolean
        case expr : Plus => tcExpr(expr.lhs,TInt,TString); tcExpr(expr.rhs,TInt,TString);
            (expr.lhs.getType,expr.rhs.getType) match {
              case (TInt,TInt) => TInt
              case (TString,TInt) => TString
              case (TInt,TString) => TString
              case (TString,TString) => TString
              case (_,_) => error("Plus Operator Invalid Overload", expr); TError
            }
        case expr : Minus => tcExpr(expr.lhs,TInt); tcExpr(expr.rhs,TInt); TInt
        case expr : Times => tcExpr(expr.lhs,TInt); tcExpr(expr.rhs,TInt); TInt
        case expr : Div => tcExpr(expr.lhs,TInt); tcExpr(expr.rhs,TInt); TInt
        case expr : LessThan => tcExpr(expr.lhs,TInt); tcExpr(expr.rhs,TInt); TBoolean
        case expr : Equals => 
            tcExpr(expr.lhs,  TInt,TBoolean,TString,TIntArray,anyObject);
            tcExpr(expr.rhs,  TInt,TBoolean,TString,TIntArray,anyObject);
            (expr.lhs.getType,expr.rhs.getType) match {
              case (TObject(x),TObject(y)) => TBoolean
              case (a,b) => if( a == b ) TBoolean else { error("Invalid Comparison", expr); TError }
            }
        case expr : ArrayRead => tcExpr(expr.arr,TIntArray); tcExpr(expr.index,TInt); TInt
        case expr : ArrayLength => tcExpr(expr.arr,TIntArray); TInt;

        //TODO: Finish this. ATTACH PROPER SYMBOL TO THE ID TAG FOR THE PRINTER.
        case expr : MethodCall => tcExpr(expr.obj,anyObject) match { //obj,meth,args
            case a : TObject => {
                a.classSymbol.lookupMethod(expr.meth.value) match {
                  case Some(m) => 
                    if(m.argList.length != expr.args.length)
                      { error("wrong number of arguments",expr); TError }
                    else {
                      m.argList.zip(expr.args).foreach {case (c,d) => tcExpr(d,c.getType) }
                      expr.meth.setSymbol(m);
                      m.getType
                    }
                  case None => error("method not found",expr.obj); TError
                };
            }
            case _ => error("variable not an object!",expr); TError
        }
        case expr : IntLit => TInt
        case expr : StringLit => TString
        case expr : True => TBoolean
        case expr : False => TBoolean
        case expr : Identifier => expr.getType
        case expr : Self  => expr.getSymbol.getType
        case expr : NewIntArray => tcExpr(expr.size,TInt); TIntArray
        case expr : New => tcExpr(expr.tpe,anyObject)
        case expr : Not => tcExpr(expr.expr,TBoolean); TBoolean
        case expr : Block => expr.exprs.foreach( x => tcExpr(x) ); tcExpr(expr.exprs.last)
        case expr : If => { tcExpr(expr.expr,TBoolean); val t = tcExpr(expr.thn);
            expr.els match { case Some(e) => tcExpr(e,t); t case None => TUnit}
        }
        case expr : While => tcExpr(expr.cond,TBoolean); tcExpr(expr.body,TUnit); TUnit;
        case expr : Println => tcExpr(expr.expr,TString); TUnit
        case expr : Assign => val t = tcExpr(expr.id); tcExpr(expr.expr,t); TUnit
        case expr : ArrayAssign => tcExpr(expr.index,TInt); tcExpr(expr.id);
            tcExpr(expr.expr,TInt); TUnit
        case expr : Strof => tcExpr(expr.expr,TInt,TBoolean); TString //can add objects here?
      }

      expr.setType(tpe)

      // Check result and return a valid type in case of error
      if (expected.isEmpty) {
        tpe
      } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
        error("Type error: expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
        expected.head
      } else {
        tpe
      }
    }

    def tcProgram(prog: Program): Unit = {
      prog.classes.foreach( c => tcClass(c) )
      tcMainMethod(prog.main)
    }

    def tcMainMethod(mthd : MainMethod) {
      mthd.main.exprs.foreach( x => tcExpr(x) )
      tcExpr(mthd.main.retExpr,TUnit)
    }

    def tcClass(clss: ClassDecl) : Unit = {
      clss.methods.foreach(m => tcMethod(m))
    }

    def tcMethod(mthd: MethodDecl) : Unit = {
      mthd.exprs.foreach( x => tcExpr(x) )
      tcExpr(mthd.retExpr,mthd.retType.getType)
    }

    //tcExpr for all exprs!
    //attach types to all expression subtrees.
    //attach proper method calls (might not be run type accurate).
    
    tcProgram(prog)
    terminateIfErrors

    prog
  }

}
