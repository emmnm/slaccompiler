package slacc
package ast

import utils._
import analyzer.Symbols._

object Trees {
  sealed trait Tree extends Positioned

  case class Program(main: MainMethod, classes: List[ClassDecl]) extends Tree
  // Note: we attach a `ClassSymbol`, because the main method should be put into a (synthetic) class called "Main";
  // the attached `ClassSymbol` is then the symbol of this "Main" class.
  case class MainMethod(main: MethodDecl) extends Tree with Symbolic[ClassSymbol] {
    val id = Identifier("Main")
    def exprs: List[ExprTree] = main.exprs ::: (main.retExpr :: Nil)
  }
  case class ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl]) extends Tree with Symbolic[ClassSymbol]
  case class VarDecl(tpe: TypeTree, id: Identifier) extends Tree with Symbolic[VariableSymbol]
  case class MethodDecl(retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], exprs: List[ExprTree], retExpr: ExprTree) extends Tree with Symbolic[MethodSymbol] {
  }
  sealed case class Formal(tpe: TypeTree, id: Identifier) extends Tree with Symbolic[VariableSymbol]

  sealed trait TypeTree extends Tree
  case class IntArrayType() extends TypeTree
  case class IntType() extends TypeTree
  case class BooleanType() extends TypeTree
  case class StringType() extends TypeTree
  case class UnitType() extends TypeTree

  sealed trait ExprTree extends Tree
  case class And(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Or(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Times(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Div(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class LessThan(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Equals(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class ArrayRead(arr: ExprTree, index: ExprTree) extends ExprTree
  case class ArrayLength(arr: ExprTree) extends ExprTree
  case class MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) extends ExprTree
  case class IntLit(value: Int) extends ExprTree
  case class StringLit(value: String) extends ExprTree

  case class True() extends ExprTree
  case class False() extends ExprTree
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
}
