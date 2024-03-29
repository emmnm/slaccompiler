package slacc
package analyzer

import utils._
import Types._

object Symbols {
  trait Symbolic[S <: Symbol] {
    private var _sym: Option[S] = None

    def setSymbol(sym: S): this.type = {
      _sym = Some(sym)
      this
    }

    def getSymbol: S = _sym match {
      case Some(s) => s
      case None => sys.error("Accessing undefined symbol.")
    }
  }

  sealed abstract class Symbol extends Positioned with Typed {
    val id: Int = ID.next
    val name: String
  }

  private object ID {
    private var c: Int = 0

    def next: Int = {
      val ret = c
      c = c + 1
      ret
    }
  }

  class GlobalScope {
    var mainClass: ClassSymbol = _
    var classes = Map[String, ClassSymbol]()

    def lookupClass(n: String): Option[ClassSymbol] = 
        if(n != mainClass.name) classes get n else Some(mainClass)
  }

  class ClassSymbol(val name: String) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods = Map[String, MethodSymbol]()
    var members = Map[String, VariableSymbol]()

    def lookupMethod(n: String): Option[MethodSymbol] = 
        parent match {
          case Some(p) => if( methods contains n) methods get n else p.lookupMethod(n)
          case None => methods get n
        }

    def lookupVar(n: String): Option[VariableSymbol] =
        parent match {
          case Some(p) => if( members contains n) members get n else p.lookupVar(n)
          case None => members get n
        }

  }

  class MethodSymbol(val name: String, val classSymbol: ClassSymbol) extends Symbol {
    var params = Map[String, VariableSymbol]()
    var members = Map[String, VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var overridden: Option[MethodSymbol] = None

    def lookupVar(n: String): Option[VariableSymbol] = //members -> param -> class.
      if(members contains n) members get n
      else if(params contains n) params get n
      else classSymbol.lookupVar(n)
  }

  class VariableSymbol(val name: String) extends Symbol
}
