package slacc
package analyzer

import Symbols._

object Types {
  trait Typed {
    self =>

    private var _tpe: Type = TUntyped

    def setType(tpe: Type): self.type = { _tpe = tpe; this }
    def getType: Type = _tpe
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case _ => false
    }
    override def toString = "Int"
  }

  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBoolean => true
      case _ => false
    }
    override def toString = "TBoolean"
  }

  case object TIntArray extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TIntArray => true
      case _ => false
    }
    override def toString = "TInt[]"
  }

  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case _ => false
    }
    override def toString = "TString"
  }

  case object TUnit extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TUnit => true
      case _ => false
    }
    override def toString = "TUnit"
  }

  case class TObject(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = 
      tpe match {
        case t : TObject => {
          if(classSymbol == t.classSymbol) true
          else classSymbol.parent match {
            case Some(par) => par.getType.isSubTypeOf(tpe)
            case None => anyObject.classSymbol.name == t.classSymbol.name
          }
        }
        case _ => false
      }

    override def toString = classSymbol.name
  }

  // special object to implement the fact that all objects are its subclasses
  val anyObject = TObject(new ClassSymbol("Object"))
}
