package slacc
package ast

import Trees._

object Printer {

  var doSymbols = false

  def apply(t: Tree): String = {
    t match {
    case t: Program => t.classes.map(apply).mkString("\n") + "\n" + apply(t.main)

    case t: MainMethod => apply(t.main)

    case t: ClassDecl => "class " + apply(t.id) + 
                            (t.parent match {case Some(e) => " <: " + apply(e) case None => "" }) +
                            " {\n\n" + indent(t.vars.map(apply).mkString("\n") + "\n\n" +
                            t.methods.map(apply).mkString("\n\n")) + "\n\n}"

 
    case t: VarDecl => "var " + apply(t.id) + " : " + apply(t.tpe) + ";"

    case t: MethodDecl => "method " + apply(t.id) + "(" +
                            t.args.map(apply).mkString(", ") +
                            ") : " + apply(t.retType) + " = {\n"  +
                            indent(     t.vars.map(apply).mkString("\n") + // all vars new lines.
                                        (if(t.vars.isEmpty) "" else "\n\n") + 
                                        (t.exprs :+ t.retExpr).map(apply).mkString(";\n")) +
                            "\n}"
 
    case t: Formal => apply(t.id) + " : " + apply(t.tpe)

    case t: IntArrayType => "Int[]" 
    case t: IntType => "Int" 
    case t: BooleanType => "Bool" 
    case t: StringType => "String" 
    case t: UnitType => "Unit" 

    case t: And =>      "(" + apply(t.lhs) + " && " + apply(t.rhs) + ")"
    case t: Or =>       "(" + apply(t.lhs) + " || " + apply(t.rhs) + ")"
    case t: Plus =>     "(" + apply(t.lhs) + " + "  + apply(t.rhs) + ")"
    case t: Minus =>    "(" + apply(t.lhs) + " - "  + apply(t.rhs) + ")"
    case t: Times =>    "(" + apply(t.lhs) + " * "  + apply(t.rhs) + ")"
    case t: Div =>      "(" + apply(t.lhs) + " / "  + apply(t.rhs) + ")"
    case t: LessThan => "(" + apply(t.lhs) + " < "  + apply(t.rhs) + ")"
    case t: Equals =>   "(" + apply(t.lhs) + " == " + apply(t.rhs) + ")"
    case t: ArrayRead => apply(t.arr) + "[" + apply(t.index) + "]"
    case t: ArrayLength => apply(t.arr) + ".length"


    case t: MethodCall => apply(t.obj) + "." + t.meth.value + (if(doSymbols) "#??" else "") + "(" + t.args.map(apply).mkString(",") + ")"

/*    case t: MethodCall => apply(t.obj) + "." + apply(t.meth) + "(" + t.args.map(apply).mkString(",") + ")" */
    case t: IntLit => t.value.toString
    case t: StringLit => "\"" + t.value + "\""
    

    case t: True => "true" 
    case t: False => "false" 
    case t: Identifier => t.value + (if(doSymbols) { "#" + t.getSymbol.id} else "")
    case t: Self => "self" 
    case t: NewIntArray => "new Int[" + apply(t.size) + "]"
    case t: New => "new " + apply(t.tpe) + "()"
    case t: Not => "!" + apply(t.expr)
    
    case t: Block => "\n{" + indent("\n" + t.exprs.map(apply).mkString(";\n")) + "\n}"
    case t: If => "if(" + apply(t.expr) + ") " + apply(t.thn) +
                    (t.els match { case Some(e) => "\nelse " + apply(e) case None => "" })
    case t: While => "while(" + apply(t.cond) + ")" + apply(t.body)
    case t: Println => "println(" + apply(t.expr) + ")"
    case t: Assign => apply(t.id) + " = " + apply(t.expr) 
    case t: ArrayAssign => apply(t.id) + "[" + apply(t.index) + "]=" + apply(t.expr) 
    case t: Strof => "strOf(" + apply(t.expr) + ")"

    }
  }

  /*Replaces all \n's with \n  */
  def indent(s: String): String = {
    "  " + s.replaceAll("\n","\n  ")
  }
}
